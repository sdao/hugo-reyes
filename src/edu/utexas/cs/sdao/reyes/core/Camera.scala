package edu.utexas.cs.sdao.reyes.core

import math._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import java.awt.RenderingHints

/**
 * A pinhole camera projection.
 * The default parameters create a camera pointing along the negative Z-axis.
 * @param rotation the rotation of the camera in world-space
 * @param translation the translation of the camera in world-space
 * @param fieldOfView the camera's horizontal field of view in radians
 * @param width the width of the output image plane
 * @param height the height of the output image plane
 * @param supersample the supersampling factor
 */
case class Camera(rotation: Vector3 = Vector3.ZERO,
                  translation: Vector3 = Vector3.ZERO,
                  fieldOfView: Float = toRadians(60.0).toFloat,
                  width: Int = 800,
                  height: Int = 600,
                  supersample: Int = 1) {

  val aspect = width.toFloat/height.toFloat

  // This matrix first undoes a translation, and then undoes a rotation.
  val worldToCamera = Matrix4.translation(-translation).antiRotate(rotation)

  private lazy val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  private lazy val zBuffer = new ZBuffer(width, height)

  private val lock : AnyRef = new Object()

  val focalLength = (width / 2.0f) / tan(fieldOfView / 2.0f).toFloat // Trigonometry, what's that?!
  val halfWidth = width / 2.0f
  val halfHeight = height / 2.0f

  /**
   * Projects a point into the image space defined by u=0..width and v=0..height.
   * @param u the vector to project
   * @return the projected vector
   */
  def project(u: Vector3): Vector2 = {
    val v = worldToCamera * u
    if (v.z == 0.0f)
      Vector2(focalLength * v.x / Float.MinPositiveValue + halfWidth,
        focalLength * v.y / Float.MinPositiveValue + halfHeight)
    else
      Vector2(focalLength * v.x / -v.z + halfWidth, // Note: negate z because we are using a right-handed system,
        focalLength * v.y / -v.z + halfHeight)      // where the camera points to -z.
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
      .expand(Vector3(newLow.x, newLow.y, b.lowBound.z))
      .expand(Vector3(newUp.x, newUp.y, b.upBound.z))
  }

  /**
   * Determines if a certain projected bounding box is contained within the camera's
   * view frustum, at least partially.
   * The projected bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be in scene world space.
   * This will also clip objects between the camera and the near plane.
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
    Camera(rotation,
      translation,
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