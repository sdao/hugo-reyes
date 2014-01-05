package edu.utexas.cs.sdao.reyes.core

import scala.math._

case class Vector2(x: Float, y: Float) {

  def +(r: Vector2) = Vector2(x + r.x, y + r.y)

  def +(r: Float) = Vector2(x + r, y + r)

  def -(r: Vector2) = Vector2(x - r.x, y - r.y)

  def -(r: Float) = Vector2(x - r, y - r)

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
}

case class Vector3(x: Float, y: Float, z: Float) {

  def +(r: Vector3) = Vector3(x + r.x, y + r.y, z + r.z)

  def +(r: Float) = Vector3(x + r, y + r, z + r)

  def -(r: Vector3) = Vector3(x - r.x, y - r.y, z - r.z)

  def -(r: Float) = Vector3(x - r, y - r, z - r)

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

  def *(xform: Matrix4): Vector3 = {
    Vector3(x * xform(0, 0) + y * xform(0, 1) + z * xform(0, 2) + xform(0, 3),
      x * xform(1, 0) + y * xform(1, 1) + z * xform(1, 2) + xform(1, 3),
      x * xform(2, 0) + y * xform(2, 1) + z * xform(2, 2) + xform(2, 3))
  }

}

case class Vector4(x: Float, y: Float, z: Float, w: Float) {

  def +(r: Vector4) = Vector4(x + r.x, y + r.y, z + r.z, w + r.w)

  def +(r: Float) = Vector4(x + r, y + r, z + r, w + r)

  def -(r: Vector4) = Vector4(x - r.x, y - r.y, z - r.z, w - r.w)

  def -(r: Float) = Vector4(x - r, y - r, z - r, w - r)

  def *(r: Vector4) = Vector4(x * r.x, y * r.y, z * r.z, w * r.w)

  def *(r: Float) = Vector4(x * r, y * r, z * r, w * r)

  def /(r: Vector4) = Vector4(x / r.x, y / r.x, z / r.z, w / r.w)

  def /(r: Float) = Vector4(x / r, y / r, z / r, w / r)

  def dot(r: Vector4) = x * r.x + y * r.y + z * r.z + w * r.w

  def length = sqrt(x * x + y * y + z * z + w * w).toFloat

  def dist(r: Vector4) = (r - this).length

  def *(xform: Matrix4): Vector4 = {
    Vector4(x * xform(0, 0) + y * xform(0, 1) + z * xform(0, 2) + xform(0, 3),
      x * xform(1, 0) + y * xform(1, 1) + z * xform(1, 2) + xform(1, 3),
      x * xform(2, 0) + y * xform(2, 1) + z * xform(2, 2) + xform(2, 3),
      w)
  }

}