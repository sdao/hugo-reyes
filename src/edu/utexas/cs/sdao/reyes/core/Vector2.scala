package edu.utexas.cs.sdao.reyes.core

import scala.math._

/**
 * A vector in 2-space.
 * @param x the first coordinate
 * @param y the second coordinate
 */
case class Vector2(x: Float, y: Float) {

  def +(r: Vector2) = Vector2(x + r.x, y + r.y)

  def +(r: Float) = Vector2(x + r, y + r)

  def -(r: Vector2) = Vector2(x - r.x, y - r.y)

  def -(r: Float) = Vector2(x - r, y - r)

  def unary_- = Vector2(-x, -y)

  def *(r: Vector2) = Vector2(x * r.x, y * r.y)

  def *(r: Float) = Vector2(x * r, y * r)

  def /(r: Vector2) = Vector2(x / r.x, y / r.x)

  def /(r: Float) = Vector2(x / r, y / r)

  /**
   * Performs the dot (scalar) product with another vector.
   * @param r the right-hand vector
   * @return the dot product
   */
  def dot(r: Vector2) = x * r.x + y * r.y

  /**
   * The length of the vector.
   * @return the length
   */
  def length = sqrt(x * x + y * y).toFloat

  /**
   * The distance from this vector to another vector.
   * @param r the other vector
   * @return the distance
   */
  def dist(r: Vector2) = (r - this).length

  /**
   * Normalizes this vector to produce a new vector with the same direction and a length of 1.
   * @return a unit vector
   */
  def normalize = if (length == 0) this else Vector2(x / length, y / length)

  def >(r: Vector2) = x > r.x && y > r.y

  def >=(r: Vector2) = x >= r.x && y >= r.y

  def <(r: Vector2) = x < r.x && y < r.y

  def <=(r: Vector2) = x <= r.x && y <= r.y

}

object Vector2 {

  /**
   * The zero vector in 2-space.
   */
  val ZERO = Vector2(0.0f, 0.0f)

}
