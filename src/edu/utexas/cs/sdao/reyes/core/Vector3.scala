package edu.utexas.cs.sdao.reyes.core

import scala.math._

/**
 * A vector in 3-space.
 * @param x the first coordinate
 * @param y the second coordinate
 * @param z the third coordinate
 */
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

  /**
   * Performs the dot (scalar) product with another vector.
   * @param r the right-hand vector
   * @return the dot product
   */
  def dot(r: Vector3) = x * r.x + y * r.y + z * r.z

  /**
   * Performs the cross (vector) product with another vector.
   * Note: this operation is not commutative.
   * @param r the right-hand vector
   * @return the cross product
   */
  def cross(r: Vector3) = Vector3(
    y * r.z - z * r.y,
    z * r.x - x * r.z,
    x * r.y - y * r.x
  )

  /**
   * The length of the vector.
   * @return the length
   */
  def length = sqrt(x * x + y * y + z * z).toFloat

  /**
   * The distance from this vector to another vector.
   * @param r the other vector
   * @return the distance
   */
  def dist(r: Vector3) = (r - this).length

  /**
   * Normalizes this vector to produce a new vector with the same direction and a length of 1.
   * @return a unit vector
   */
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
   * Returns the 2-D distance between this vector and another vector
   * by discarding the Z-coordinate.
   * @param r the other vector
   * @return the 2-D distance
   */
  def dist2D(r: Vector3) = sqrt(pow(x - r.x, 2.0f) + pow(y - r.y, 2.0f)).toFloat

}

object Vector3 {

  /**
   * The zero vector in 3-space.
   */
  val ZERO = Vector3(0.0f, 0.0f, 0.0f)

  val I = Vector3(1.0f, 0.0f, 0.0f)
  val J = Vector3(0.0f, 1.0f, 0.0f)
  val K = Vector3(0.0f, 0.0f, 1.0f)

  val NegativeI = Vector3(-1.0f, 0.0f, 0.0f)
  val NegativeJ = Vector3(0.0f, -1.0f, 0.0f)
  val NegativeK = Vector3(0.0f, 0.0f, -1.0f)

}
