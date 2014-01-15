package edu.utexas.cs.sdao.reyes.core

import math._

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
}

object Camera {
  val PROJECT_OFFSET = Vector2(1.0f, 1.0f)
}