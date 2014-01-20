package edu.utexas.cs.sdao.reyes.core

import math._
import MathHelpers._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import java.awt.RenderingHints

/**
 * A pinhole camera projection.
 * The default parameters create a camera pointing along the negative Z-axis.
 * @param worldToCamera a transformation matrix that converts world coordinates
 *                      to camera coordinates; e.g., if the camera is translated one unit
 *                      to the left, then this matrix will move world objects one unit to
 *                      the right
 * @param fieldOfView the camera's horizontal field of view in radians,
 *                    measured as the angle from the left of the visible screen to the right;
 *                    acceptable values are 0 < fieldOfView < Pi
 * @param width the width of the output image plane
 * @param height the height of the output image plane
 * @param supersample the supersampling factor
 */
case class Camera(worldToCamera: Matrix4 = Matrix4.IDENTITY,
                  fieldOfView: Float = toRadians(60.0).toFloat,
                  width: Int = 800,
                  height: Int = 600,
                  supersample: Int = 1) {

  if (fieldOfView <= 0.0 || fieldOfView >= Pi)
    throw  new IllegalArgumentException("fieldOfView out of range")

  val aspect = width.toFloat/height.toFloat

  private lazy val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  lazy val zBuffer = new ZBuffer(width, height)

  private val lock : AnyRef = new Object()

  val focalLength = (width / 2.0f) / tan(fieldOfView / 2.0f).toFloat // Trigonometry, what's that?!
  val halfWidth = width / 2.0f
  val halfHeight = height / 2.0f

  /**
   * Projects a point into the image space defined by u=0..width and v=0..height.
   * The z-component is the z-depth of the point.
   * @param u the vector to project
   * @return the projected vector
   */
  def project(u: Vector3): Vector3 = {
    val v = worldToCamera * u
    if (v.z == 0.0f)
      Vector3(focalLength * v.x / Float.MinPositiveValue + halfWidth,
        focalLength * v.y / Float.MinPositiveValue + halfHeight,
        v.z)
    else
      Vector3(focalLength * v.x / -v.z + halfWidth, // Note: negate z because we are using a right-handed system,
        focalLength * v.y / -v.z + halfHeight,      // where the camera points to -z.
        v.z)
  }

  /**
   * Projects the points of a bounding box onto the image space defined by u=0..width and
   * v=0..height.
   * @param b the bounding box to project
   * @return the projected bounding box
   */
  def project(b: FilledBoundingBox): FilledBoundingBox = {
    val newLow = project(b.lowBound)
    val newUp = project(b.upBound)
    BoundingBox.empty
      .expand(newLow)
      .expand(newUp)
  }

  /**
   * Determines if a certain projected bounding box is contained within the camera's
   * view frustum, at least partially.
   * The projected bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be in scene world space.
   * This will also clip objects between the camera and the near plane.
   * @param boundingBox the bounding box to check
   * @return the visibility of the bounding box
   */
  def containsBoundingBox(boundingBox: BoundingBox): Boolean = {
    boundingBox match {
      case EmptyBoundingBox() => false
      case FilledBoundingBox(lowBound, upBound) =>
        upBound.x >= 0.0f &&
          lowBound.x <= width &&
          upBound.y >= 0.0f &&
          lowBound.y <= height &&
          lowBound.z < 0.0f
    }
  }

  /**
   * Estimates whether the z-buffer occludes a projected bounding box.
   * The projected bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be in scene world space.
   *
   * Since this function is used to prevent the final projection and shading
   * of hidden surfaces, it assumes that the bounding box intersects the screen
   * and that the bounding box is in non-supersampled screen coordinates,
   * even if the camera is supersampling.
   *
   * @param boundingBox the bounding box to check
   * @return whether the bounding box is occluded
   */
  def estimateZBufferOcclusion(boundingBox: BoundingBox): Boolean = {
    boundingBox match {
      case EmptyBoundingBox() => true
      case FilledBoundingBox(lowBound, upBound) =>
        val zDepth = upBound.z

        val minX = limit(0, width - 1, floor(lowBound.x * supersample).toInt)
        val maxX = limit(0, width - 1, ceil(upBound.x * supersample).toInt)
        val minY = limit(0, height - 1, floor(lowBound.y * supersample).toInt)
        val maxY = limit(0, height - 1, ceil(upBound.y * supersample).toInt)

        // We're going to do this the traditional, mutable way for performance.
        var occluded = true
        var x = minX
        while (x <= maxX && occluded) {
          var y = minY
          while (y <= maxY && occluded) {
            occluded = !zBuffer.canPaint(x, buffer.getHeight - y - 1, zDepth)
            y += 1
          }
          x += 1
        }

        occluded
    }
  }

  /**
   * Creates a supersampled version of the camera.
   * The supersampled version will scale the number of pixels sampled by the square
   * of the rate given (the rate represents the multiplicative increase per axis).
   *
   * Note: do not use supersampled cameras during the splitting phase. Split first,
   * and then use the supersampled camera in the rest of the render pipeline.
   * @param rate the supersampling factor
   * @return the new camera, adjusted for supersampling
   */
  def getSupersampledCamera(rate: Int): Camera = {
    Camera(worldToCamera,
      fieldOfView,
      width * rate,
      height * rate,
      supersample * rate)
  }

  /**
   * Renders part of the image using a rendering function.
   * A lock will be acquired before the rendering function begins in
   * order to keep the image buffer and z-buffer thread-safe.
   * It is thus best to limit the amount of time spent in the rendering function.
   * @param func a function that takes an image buffer and z-buffer and renders to them
   * @return the result of the inner function
   */
  def render(func: (BufferedImage, ZBuffer) => Any) = {
    lock.synchronized { func(buffer, zBuffer) }
  }

  /**
   * Returns the image buffer as-is.
   * If the camera is supersampling, the image buffer's dimensions will be
   * scaled up by the supersampling factor.
   * @return the image buffer
   */
  def image = buffer

  /**
   * Returns the buffer image downsized to the real width and height if the
   * camera is a supersampling camera. Otherwise, the image buffer is
   * returned as-is.
   * @return the image resized to its real dimensions
   */
  def realImage = {
    if (supersample == 1)
      buffer
    else {
      val newImage = new BufferedImage(realWidth, realHeight, buffer.getType)
      
      val g2 = newImage.createGraphics()
      g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g2.drawImage(buffer, 0, 0, realWidth, realHeight, null)
      g2.dispose()

      newImage
    }
  }

  /**
   * The real width of the output image, in pixels.
   * If the camera is not supersampling, then this is just the width.
   * If the camera is supersampling, this is the width divided by the supersampling factor.
   * @return
   */
  def realWidth = width / supersample

  /**
   * The real height of the output image, in pixels.
   * If the camera is not supersampling, then this is just the height.
   * If the camera is supersampling, this is the height divided by the supersampling factor.
   * @return
   */
  def realHeight = height / supersample

  /**
   * Writes the real-sized image to a PNG file.
   * @param name the name of the image file, preferably ending in ".png"
   * @return whether the write succeeded
   */
  def writeImageFile(name: String) = {
    val f = new File(name)
    f.createNewFile()
    ImageIO.write(realImage, "png", f)
  }
}

object Camera {
  val PROJECT_OFFSET = Vector2(1.0f, 1.0f)
}