package edu.utexas.cs.sdao.reyes.core

import math._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

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
 */
case class Camera(rotation: Vector3 = Vector3.ZERO,
                  translation: Vector3 = Vector3.ZERO,
                  fieldOfView: Float = toRadians(60.0).toFloat,
                  width: Int = 800,
                  height: Int = 600,
                  near: Float = 1.0f,
                  far: Float = 100.0f) {

  val aspect = width.toFloat/height.toFloat

  // This matrix first undoes a translation, and then undoes a rotation.
  val worldToCamera = Matrix4.translation(-translation).antiRotate(rotation)
  val cameraToScreen = Matrix4.perspective(fieldOfView, aspect, near, far)

  val xform = cameraToScreen * worldToCamera

  private val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  private val zBuffer = new ZBuffer(width, height)

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

  def project(b: FilledBoundingBox): FilledBoundingBox = {
    val newLow = project(b.lowBound)
    val newUp = project(b.upBound)
    FilledBoundingBox(Vector3(newLow.x, newLow.y, b.lowBound.z),
      Vector3(newUp.x, newUp.y, b.upBound.z))
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
          -lowBound.z >= near /* must negate z, since using the OpenGL convention
                                 of pointing towards the negative z-axis */
    }
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

  def image = buffer

  def writeImageFile(name: String) = {
    val f = new File(name)
    f.createNewFile()
    ImageIO.write(buffer, "png", f)
  }
}

object Camera {
  val PROJECT_OFFSET = Vector2(1.0f, 1.0f)
}