package edu.utexas.cs.sdao.reyes.core

import scala.math._

/**
 * Defines a 4x4 matrix.
 */
class Matrix4(val data: Array[Float] = Array.ofDim[Float](16)) {

  if (data.length != 16)
    throw new IllegalArgumentException("Dim of matrix array != 16")

  private def idx(row: Int, col: Int) = row * 4 + col

  private def mult(arr1: Array[Float], arr2: Array[Float]) = {
    val newData = Array.ofDim[Float](16)

    for (i <- 0 until 4) { // i = row
      for (j <- 0 until 4) { // j = col
        newData(idx(i, j)) =
          arr1(idx(i, 0)) * arr2(idx(0, j)) +
            arr1(idx(i, 1)) * arr2(idx(1, j)) +
            arr1(idx(i, 2)) * arr2(idx(2, j)) +
            arr1(idx(i, 3)) * arr2(idx(3, j))
      }
    }

    newData
  }

  def *(right: Matrix4) = new Matrix4(mult(data, right.data))

  def *(u: Vector3): Vector3 = {
    Vector3(u.x * this(0, 0) + u.y * this(0, 1) + u.z * this(0, 2) + this(0, 3),
      u.x * this(1, 0) + u.y * this(1, 1) + u.z * this(1, 2) + this(1, 3),
      u.x * this(2, 0) + u.y * this(2, 1) + u.z * this(2, 2) + this(2, 3))
  }

  def *(u: Vector4): Vector4 = {
    Vector4(u.x * this(0, 0) + u.y * this(0, 1) + u.z * this(0, 2) + u.w * this(0, 3),
      u.x * this(1, 0) + u.y * this(1, 1) + u.z * this(1, 2) + u.w * this(1, 3),
      u.x * this(2, 0) + u.y * this(2, 1) + u.z * this(2, 2) + u.w * this(2, 3),
      u.x * this(3, 0) + u.y * this(3, 1) + u.z * this(3, 2) + u.w * this(3, 3))
  }

  def rotate(rads: Vector3) = new Matrix4(mult(Matrix4.rotation(rads).data, data))

  def antiRotate(rads: Vector3) = new Matrix4(mult(Matrix4.antiRotation(rads).data, data))

  def rotateX(rads: Float) = new Matrix4(mult(Matrix4.rotationX(rads).data, data))

  def rotateY(rads: Float) = new Matrix4(mult(Matrix4.rotationY(rads).data, data))

  def rotateZ(rads: Float) = new Matrix4(mult(Matrix4.rotationZ(rads).data, data))

  def translate(t: Vector3) = new Matrix4(mult(Matrix4.translation(t).data, data))

  def scale(s: Vector3) = new Matrix4(mult(Matrix4.scaling(s).data, data))

  def apply(row: Int, col: Int) = data(idx(row, col))

  override def toString: String = {
    val m00 = apply(0, 0)
    val m01 = apply(0, 1)
    val m02 = apply(0, 2)
    val m03 = apply(0, 3)

    val m10 = apply(1, 0)
    val m11 = apply(1, 1)
    val m12 = apply(1, 2)
    val m13 = apply(1, 3)

    val m20 = apply(2, 0)
    val m21 = apply(2, 1)
    val m22 = apply(2, 2)
    val m23 = apply(2, 3)

    val m30 = apply(3, 0)
    val m31 = apply(3, 1)
    val m32 = apply(3, 2)
    val m33 = apply(3, 3)

    f"Matrix4( \n$m00%8.3f $m01%8.3f $m02%8.3f $m03%8.3f \n$m10%8.3f $m11%8.3f $m12%8.3f $m13%8.3f \n$m20%8.3f $m21%8.3f $m22%8.3f $m23%8.3f \n$m30%8.3f $m31%8.3f $m32%8.3f $m33%8.3f \n)"
  }
}

object Matrix4 {

  val IDENTITY = new Matrix4(Array(1.0f, 0.0f, 0.0f, 0.0f,
                                   0.0f, 1.0f, 0.0f, 0.0f,
                                   0.0f, 0.0f, 1.0f, 0.0f,
                                   0.0f, 0.0f, 0.0f, 1.0f))

  val ZERO = new Matrix4(Array(0.0f, 0.0f, 0.0f, 0.0f,
                               0.0f, 0.0f, 0.0f, 0.0f,
                               0.0f, 0.0f, 0.0f, 0.0f,
                               0.0f, 0.0f, 0.0f, 0.0f))

  def rotationX(rads: Float) = {
    val c = cos(rads).toFloat
    val s = sin(rads).toFloat

    new Matrix4(Array(1.0f, 0.0f, 0.0f, 0.0f,
                      0.0f, c,    -s,   0.0f,
                      0.0f, s,    c,    0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f))
  }

  def rotationY(rads: Float) = {
    val c = cos(rads).toFloat
    val s = sin(rads).toFloat

    new Matrix4(Array(c,    0.0f, s,  0.0f,
                      0.0f, 1.0f, 0.0f, 0.0f,
                      -s,   0.0f, c,  0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f))
  }

  def rotationZ(rads: Float) = {
    val c = cos(rads).toFloat
    val s = sin(rads).toFloat

    new Matrix4(Array(c,    -s,   0.0f, 0.0f,
                      s,    c,    0.0f, 0.0f,
                      0.0f, 0.0f, 1.0f, 0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f))
  }

  /**
   * Generates a matrix for an Euler rotation along the
   * Z, Y, then X-axes, in that order.
   * @param rads a vector containing the rotation orders
   * @return the rotation matrix
   */
  def rotation(rads: Vector3) = {
    rotationX(rads.x) * rotationY(rads.y) * rotationZ(rads.z)
  }

  /**
   * Generates a matrix for reversing an Euler rotation along the
   * Z, Y, then X-axes, in that order.
   * In effect, the anti-rotation reverses the rotation first
   * on the X, then on the Y, and then the Z-axes.
   *
   * @param rads a vector containing the rotation orders
   * @return the rotation matrix
   */
  def antiRotation(rads: Vector3) = {
    rotationZ(rads.z) * rotationY(rads.y) * rotationX(rads.x)
  }

  def translation(amount: Vector3) = {
    new Matrix4(Array(1.0f, 0.0f, 0.0f, amount.x,
                      0.0f, 1.0f, 0.0f, amount.y,
                      0.0f, 0.0f, 1.0f, amount.z,
                      0.0f, 0.0f, 0.0f, 1.0f))
  }

  def scaling(factor: Vector3) = {
    new Matrix4(Array(factor.x, 0.0f,     0.0f,     0.0f,
                      0.0f,     factor.y, 0.0f,     0.0f,
                      0.0f,     0.0f,     factor.z, 0.0f,
                      0.0f,     0.0f,     0.0f,     1.0f))
  }

  def perspective(fieldOfViewRads: Float,
                  aspect: Float,
                  near: Float,
                  far: Float) = {
    val yMax = near * tan(fieldOfViewRads / 2.0f).toFloat
    val xMax = yMax * aspect

    new Matrix4(Array((2.0f * near)/(2.0f * xMax), 0.0f, 0.0f, 0.0f,
                      0.0f, (2.0f * near)/(2.0f * yMax), 0.0f, 0.0f,
                      0.0f, 0.0f, (near + far)/(near - far), (2.0f * near * far)/(near - far),
                      0.0f, 0.0f, -1.0f, 0.0f))
  }
}