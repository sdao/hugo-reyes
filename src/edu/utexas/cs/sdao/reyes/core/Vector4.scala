package edu.utexas.cs.sdao.reyes.core

import scala.math._

case class Vector4(x: Float, y: Float, z: Float, w: Float) {

  def +(r: Vector4) = Vector4(x + r.x, y + r.y, z + r.z, w + r.w)

  def +(r: Float) = Vector4(x + r, y + r, z + r, w + r)

  def -(r: Vector4) = Vector4(x - r.x, y - r.y, z - r.z, w - r.w)

  def -(r: Float) = Vector4(x - r, y - r, z - r, w - r)

  def unary_- = Vector4(-x, -y, -z, -w)

  def *(r: Vector4) = Vector4(x * r.x, y * r.y, z * r.z, w * r.w)

  def *(r: Float) = Vector4(x * r, y * r, z * r, w * r)

  def /(r: Vector4) = Vector4(x / r.x, y / r.x, z / r.z, w / r.w)

  def /(r: Float) = Vector4(x / r, y / r, z / r, w / r)

  def dot(r: Vector4) = x * r.x + y * r.y + z * r.z + w * r.w

  def length = sqrt(x * x + y * y + z * z + w * w).toFloat

  def dist(r: Vector4) = (r - this).length

}

object Vector4 {

  val ZERO = Vector4(0.0f, 0.0f, 0.0f, 0.0f)

}
