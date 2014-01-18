package edu.utexas.cs.sdao.reyes.core

import scala.math._

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

  def dot(r: Vector2) = x * r.x + y * r.y

  def length = sqrt(x * x + y * y).toFloat

  def dist(r: Vector2) = (r - this).length

  def normalize = if (length == 0) this else Vector2(x / length, y / length)

  def >(r: Vector2) = x > r.x && y > r.y

  def >=(r: Vector2) = x >= r.x && y >= r.y

  def <(r: Vector2) = x < r.x && y < r.y

  def <=(r: Vector2) = x <= r.x && y <= r.y

  /**
   * Clamps the x- and y- coordinates to the closed interval [-1, 1].
   * (Note: this function actually clamps to -1.001 and 1.001, to ensure
   * that off-screen objects remain off-screen.)
   * @return the clamped vector
   */
  def clamp = Vector2(max(-1.001f, min(1.001f, x)), max(-1.001f, min(1.001f, y)))

}

object Vector2 {

  val ZERO = Vector2(0.0f, 0.0f)

}
