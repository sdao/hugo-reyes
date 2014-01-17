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
 * @param fieldOfView the camera's field of view in radians
 * @param width the width of the output image plane
 * @param height the height of the output image plane
 * @param near the distance of the near plane from the camera
 * @param far the distance of the fat plane from the camera
 * @param supersample the supersampling factor
 */
case class Camera(rotation: Vector3 = Vector3.ZERO,
                  translation: Vector3 = Vector3.ZERO,
                  fieldOfView: Float = toRadians(60.0).toFloat,
                  width: Int = 800,
                  height: Int = 600,
                  near: Float = 1.0f,
                  far: Float = 100.0f,
                  supersample: Int = 1) {

  val aspect = width.toFloat/height.toFloat

  // This matrix first undoes a translation, and then undoes a rotation.
  val worldToCamera = Matrix4.translation(-translation).antiRotate(rotation)
  val cameraToScreen = Matrix4.perspective(fieldOfView, aspect, near, far)

  val xform = cameraToScreen * worldToCamera

  private lazy val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  private lazy val zBuffer = new ZBuffer(width, height)

  private val lock : AnyRef = new Object()

  /**
   * Projects a point into the square defined by u=-1..1 and v=-1..1.
   * @param u the vector to project
   * @return the projected vector
   */
  private def projectNormalized(u: Vector3): Vector2 = {
    (xform * u).toVector2
  }

  /**
   * Projects a point into the image space defined by u=0..width and v=0..height.
   * @param u the vector to project
   * @return the projected vector
   */
  def project(u: Vector3): Vector2 = {
    (projectNormalized(u) + Camera.PROJECT_OFFSET) * Vector2(width.toFloat/2.0f, height.toFloat/2.0f)
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
   * Determines if a certain bounding box is contained within the camera's
   * view frustum, at least partially.
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
          -lowBound.z >= near && // Must negate z, since using the OpenGL convention
          -lowBound.z <= far     // of pointing towards the negative z-axis.
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
      near,
      far,
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