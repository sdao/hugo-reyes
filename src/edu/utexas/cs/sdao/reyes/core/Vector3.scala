package edu.utexas.cs.sdao.reyes.core

import scala.math._

case class Vector3(x: Float, y: Float, z: Float) {

  def +(r: Vector3) = Vector3(x + r.x, y + r.y, z + r.z)

  def +(r: Float) = Vector3(x + r, y + r, z + r)

  def -(r: Vector3) = Vector3(x - r.x, y - r.y, z - r.z)

  def -(r: Float) = Vector3(x - r, y - r, z - r)

  def unary_- = Vector3(-x, -y, -z)

  def *(r: Vector3) = Vector3(x * r.x, y * r.y, z * r.z)

  def *(r: Float) = Vector3(x * r, y * r, z * r)

  def /(r: Vector3) = Vector3(x / r.x, y / r.x, z / r.z)

  def /(r: Float) = Vector3(x / r, y / r, z / r)

  def dot(r: Vector3) = x * r.x + y * r.y + z * r.z

  def cross(r: Vector3) = Vector3(
    y * r.z - z * r.y,
    z * r.x - x * r.z,
    x * r.y - y * r.x
  )

  def length = sqrt(x * x + y * y + z * z).toFloat

  def dist(r: Vector3) = (r - this).length

  def normalize = if (length == 0) this else Vector3(x / length, y / length, z / length)

  /**
   * Returns the vector (x/z, y/z),
   * assuming that (x, y, z) is a homogenous vector.
   * @return a Vector2 containing the (x/z, y/z) components
   */
  def toVector2: Vector2 =
    if (z != 0.0f)
      Vector2(x/z, y/z)
    else
      Vector2.ZERO

  /**
   * Returns the vector (x, y), discarding the z-component.
   * This method differs from toVector2 in that it does not
   * adjust by the scalar for homogenous coordinates.
   * @return a Vector2 containing the (x, y) components
   */
  def truncateToVector2: Vector2 = Vector2(x, y)

}

object Vector3 {
  val ZERO = Vector3(0.0f, 0.0f, 0.0f)
  val I = Vector3(1.0f, 0.0f, 0.0f)
  val J = Vector3(0.0f, 1.0f, 0.0f)
  val K = Vector3(0.0f, 0.0f, 1.0f)
}
