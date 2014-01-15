package edu.utexas.cs.sdao.reyes.core

import math._

/**
 * A pinhole camera projection.
 * The default parameters create a camera pointing along the negative Z-axis.
 * @param rotation
 * @param translation
 * @param fieldOfView
 * @param aspect
 * @param near
 * @param far
 */
case class Camera(rotation: Vector3 = Vector3.ZERO,
                  translation: Vector3 = Vector3.ZERO,
                  fieldOfView: Float = toRadians(60.0).toFloat,
                  aspect: Float = 8.0f/6.0f,
                  near: Float = 1.0f,
                  far: Float = 100.0f) {

  // This matrix first undoes a translation, and then undoes a rotation.
  val worldToCamera = Matrix4.translation(-translation).antiRotate(rotation)
  val cameraToScreen = Matrix4.perspective(fieldOfView, aspect, near, far)

  val xform = cameraToScreen * worldToCamera

  def project(u: Vector3): Vector2 = {
    (xform * u).toVector2
  }
}
