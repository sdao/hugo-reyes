package edu.utexas.cs.sdao.reyes.core

import scala.math._

/**
 * Defines a 4x4 matrix.
 * @param data a 16-item array containing the matrix in row-major
 *             order, i.e. indices 0-3 are the first row, 4-7 the second row, etc.
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

  /**
   * Generates the inverse matrix, i.e. a matrix that,
   * when left- or right-multiplied with this one, produces the
   * identity matrix.
   *
   * Use this function to produce a transformation matrix that will
   * undo a given set of transformations.
   *
   * If this matrix is singular, then an inverse does not exist,
   * so the original matrix will be returned.
   *
   * Note: this implementation doesn't do anything fancy.
   * It hard-codes the arithmetic for each new matrix value.
   *
   * @return the inverse matrix
   */
  def invert: Matrix4 = {
    val newData = Array.ofDim[Float](16)

    newData(0) = data(5)  * data(10) * data(15) -
      data(5)  * data(11) * data(14) -
      data(9)  * data(6)  * data(15) +
      data(9)  * data(7)  * data(14) +
      data(13) * data(6)  * data(11) -
      data(13) * data(7)  * data(10)

    newData(4) = -data(4)  * data(10) * data(15) +
      data(4)  * data(11) * data(14) +
      data(8)  * data(6)  * data(15) -
      data(8)  * data(7)  * data(14) -
      data(12) * data(6)  * data(11) +
      data(12) * data(7)  * data(10)

    newData(8) = data(4)  * data(9) * data(15) -
      data(4)  * data(11) * data(13) -
      data(8)  * data(5) * data(15) +
      data(8)  * data(7) * data(13) +
      data(12) * data(5) * data(11) -
      data(12) * data(7) * data(9)

    newData(12) = -data(4)  * data(9) * data(14) +
      data(4)  * data(10) * data(13) +
      data(8)  * data(5) * data(14) -
      data(8)  * data(6) * data(13) -
      data(12) * data(5) * data(10) +
      data(12) * data(6) * data(9)

    newData(1) = -data(1)  * data(10) * data(15) +
      data(1)  * data(11) * data(14) +
      data(9)  * data(2) * data(15) -
      data(9)  * data(3) * data(14) -
      data(13) * data(2) * data(11) +
      data(13) * data(3) * data(10)

    newData(5) = data(0)  * data(10) * data(15) -
      data(0)  * data(11) * data(14) -
      data(8)  * data(2) * data(15) +
      data(8)  * data(3) * data(14) +
      data(12) * data(2) * data(11) -
      data(12) * data(3) * data(10)

    newData(9) = -data(0)  * data(9) * data(15) +
      data(0)  * data(11) * data(13) +
      data(8)  * data(1) * data(15) -
      data(8)  * data(3) * data(13) -
      data(12) * data(1) * data(11) +
      data(12) * data(3) * data(9)

    newData(13) = data(0)  * data(9) * data(14) -
      data(0)  * data(10) * data(13) -
      data(8)  * data(1) * data(14) +
      data(8)  * data(2) * data(13) +
      data(12) * data(1) * data(10) -
      data(12) * data(2) * data(9)

    newData(2) = data(1)  * data(6) * data(15) -
      data(1)  * data(7) * data(14) -
      data(5)  * data(2) * data(15) +
      data(5)  * data(3) * data(14) +
      data(13) * data(2) * data(7) -
      data(13) * data(3) * data(6)

    newData(6) = -data(0)  * data(6) * data(15) +
      data(0)  * data(7) * data(14) +
      data(4)  * data(2) * data(15) -
      data(4)  * data(3) * data(14) -
      data(12) * data(2) * data(7) +
      data(12) * data(3) * data(6)

    newData(10) = data(0)  * data(5) * data(15) -
      data(0)  * data(7) * data(13) -
      data(4)  * data(1) * data(15) +
      data(4)  * data(3) * data(13) +
      data(12) * data(1) * data(7) -
      data(12) * data(3) * data(5)

    newData(14) = -data(0)  * data(5) * data(14) +
      data(0)  * data(6) * data(13) +
      data(4)  * data(1) * data(14) -
      data(4)  * data(2) * data(13) -
      data(12) * data(1) * data(6) +
      data(12) * data(2) * data(5)

    newData(3) = -data(1) * data(6) * data(11) +
      data(1) * data(7) * data(10) +
      data(5) * data(2) * data(11) -
      data(5) * data(3) * data(10) -
      data(9) * data(2) * data(7) +
      data(9) * data(3) * data(6)

    newData(7) = data(0) * data(6) * data(11) -
      data(0) * data(7) * data(10) -
      data(4) * data(2) * data(11) +
      data(4) * data(3) * data(10) +
      data(8) * data(2) * data(7) -
      data(8) * data(3) * data(6)

    newData(11) = -data(0) * data(5) * data(11) +
      data(0) * data(7) * data(9) +
      data(4) * data(1) * data(11) -
      data(4) * data(3) * data(9) -
      data(8) * data(1) * data(7) +
      data(8) * data(3) * data(5)

    newData(15) = data(0) * data(5) * data(10) -
      data(0) * data(6) * data(9) -
      data(4) * data(1) * data(10) +
      data(4) * data(2) * data(9) +
      data(8) * data(1) * data(6) -
      data(8) * data(2) * data(5)

    val det = data(0) * newData(0) + data(1) * newData(4) + data(2) * newData(8) + data(3) * newData(12)
    if (det == 0) {
      this
    } else {
      for (i <- 0 until 16) {
        newData(i) /= det
      }
      new Matrix4(newData)
    }
  }

  /**
   * Produces a new transformation matrix that does all of the previous transformations,
   * and then does a rotation; positive values rotate counter-clockwise.
   * In other words, a rotation matrix is left-multiplied to the current matrix.
   * @param rads the Euler angles of the rotation; rotation is performed in XYZ order
   * @return the new transformation matrix
   */
  def rotate(rads: Vector3) = new Matrix4(mult(Matrix4.rotation(rads).data, data))

  /**
   * Produces a new transformation matrix that does all of the previous transformations,
   * and then does a rotation about the positive X-axis; positive values rotate counter-clockwise.
   * In other words, a rotation matrix is left-multiplied to the current matrix.
   * @param rads the radians to rotate counter-clockwise along the +X-axis
   * @return the new transformation matrix
   */
  def rotateX(rads: Float) = new Matrix4(mult(Matrix4.rotationX(rads).data, data))

  /**
   * Produces a new transformation matrix that does all of the previous transformations,
   * and then does a rotation about the positive Y-axis; positive values rotate counter-clockwise.
   * In other words, a rotation matrix is left-multiplied to the current matrix.
   * @param rads the radians to rotate counter-clockwise along the +Y-axis
   * @return the new transformation matrix
   */
  def rotateY(rads: Float) = new Matrix4(mult(Matrix4.rotationY(rads).data, data))

  /**
   * Produces a new transformation matrix that does all of the previous transformations,
   * and then does a rotation about the positive Z-axis; positive values rotate counter-clockwise.
   * In other words, a rotation matrix is left-multiplied to the current matrix.
   * @param rads the radians to rotate counter-clockwise along the +Z-axis
   * @return the new transformation matrix
   */
  def rotateZ(rads: Float) = new Matrix4(mult(Matrix4.rotationZ(rads).data, data))

  /**
   * Produces a new transformation matrix that does all of the previous transformations,
   * and then looks in the specified direction.
   * In other words, a look-at matrix is left-multiplied to the current matrix.
   * @param dir the direction to look at, relative to the curren position
   * @return the new transformation matrix
   */
  def lookAt(dir: Vector3) = new Matrix4(mult(Matrix4.lookAt(dir).data, data))

  /**
   * Produces a new transformation matrix that does all of the previous transformations,
   * and then translates in the specified direction.
   * In other words, a translation matrix is left-multiplied to the current matrix.
   * @param t the direction to translate in
   * @return the new transformation matrix
   */
  def translate(t: Vector3) = new Matrix4(mult(Matrix4.translation(t).data, data))

  /**
   * Produces a new transformation matrix that does all of the previous transformations,
   * and then applies the specified scaling.
   * In other words, a scaling matrix is left-multiplied to the current matrix.
   * @param s the amount to scale along each axis
   * @return the new transformation matrix
   */
  def scale(s: Vector3) = new Matrix4(mult(Matrix4.scaling(s).data, data))

  /**
   * Gets the value of the matrix at the specified position.
   * @param row the row of the desired value
   * @param col the column of the desired value
   * @return the desired value
   */
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

  /**
   * The identity matrix. When any matrix M is right- or left-multiplied
   * by the identity, M is returned.
   */
  val IDENTITY = new Matrix4(Array(1.0f, 0.0f, 0.0f, 0.0f,
                                   0.0f, 1.0f, 0.0f, 0.0f,
                                   0.0f, 0.0f, 1.0f, 0.0f,
                                   0.0f, 0.0f, 0.0f, 1.0f))

  /**
   * The matrix composed of all zero values. When any matrix M is right- or
   * left-multiplied by the zero matrix, the zero matrix is returned.
   */
  val ZERO = new Matrix4(Array(0.0f, 0.0f, 0.0f, 0.0f,
                               0.0f, 0.0f, 0.0f, 0.0f,
                               0.0f, 0.0f, 0.0f, 0.0f,
                               0.0f, 0.0f, 0.0f, 0.0f))

  /**
   * Performs a counter-clockwise rotation on the positive x-axis,
   * i.e. the rotation would appear to be counter-clockwise if looking in
   * the direction of the negative x-axis.
   *
   * @param rads the angle to rotate
   * @return the rotation matrix
   */
  def rotationX(rads: Float) = {
    val c = cos(rads).toFloat
    val s = sin(rads).toFloat

    new Matrix4(Array(1.0f, 0.0f, 0.0f, 0.0f,
                      0.0f, c,    -s,   0.0f,
                      0.0f, s,    c,    0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f))
  }

  /**
   * Performs a counter-clockwise rotation on the positive y-axis,
   * i.e. the rotation would appear to be counter-clockwise if looking in
   * the direction of the negative y-axis.
   *
   * @param rads the angle to rotate
   * @return the rotation matrix
   */
  def rotationY(rads: Float) = {
    val c = cos(rads).toFloat
    val s = sin(rads).toFloat

    new Matrix4(Array(c,    0.0f, s,  0.0f,
                      0.0f, 1.0f, 0.0f, 0.0f,
                      -s,   0.0f, c,  0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f))
  }

  /**
   * Performs a counter-clockwise rotation on the positive z-axis,
   * i.e. the rotation would appear to be counter-clockwise if looking in
   * the direction of the negative z-axis.
   *
   * @param rads the angle to rotate
   * @return the rotation matrix
   */
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
   * X, Y, then Z-axes, in that order.
   * @param rads a vector containing the rotation orders
   * @return the rotation matrix
   */
  def rotation(rads: Vector3) = {
    rotationZ(rads.z) * rotationY(rads.y) * rotationX(rads.x)
  }

  /**
   * Generates a matrix for the rotation caused by looking
   * towards the specified direction from the origin.
   *
   * See [[http://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle this Wikipedia article]]
   * for the source of the equations.
   *
   * @param dir the direction to look at
   * @return the rotation matrix
   */
  def lookAt(dir: Vector3) = {
    val normalDir = dir.normalize
    val axis = (Vector3.NegativeK cross normalDir).normalize
    val c = Vector3.NegativeK dot normalDir
    val s = sqrt(1 - c * c).toFloat
    val t = 1 - c

    val data = Array.ofDim[Float](16)
    data(0) = t * axis.x * axis.x + c
    data(1) = t * axis.x * axis.y - axis.z * s
    data(2) = t * axis.x * axis.z + axis.y * s

    data(4) = t * axis.x * axis.y + axis.z * s
    data(5) = t * axis.y * axis.y + c
    data(6) = t * axis.y * axis.z - axis.x * s

    data(8) = t * axis.x * axis.z - axis.y * s
    data(9) = t * axis.y * axis.z + axis.x * s
    data(10) = t * axis.z * axis.z + c

    data(15) = 1.0f

    new Matrix4(data)
  }

  /**
   * Generates a matrix for translation in the specified distances.
   * @param amount the amount to translate along each axis
   * @return the translation matrix
   */
  def translation(amount: Vector3) = {
    new Matrix4(Array(1.0f, 0.0f, 0.0f, amount.x,
                      0.0f, 1.0f, 0.0f, amount.y,
                      0.0f, 0.0f, 1.0f, amount.z,
                      0.0f, 0.0f, 0.0f, 1.0f))
  }

  /**
   * Generates a matrix for scaling with the specified factors.
   * @param factor the factor to scale by along each axis
   * @return the scaling matrix
   */
  def scaling(factor: Vector3) = {
    new Matrix4(Array(factor.x, 0.0f,     0.0f,     0.0f,
                      0.0f,     factor.y, 0.0f,     0.0f,
                      0.0f,     0.0f,     factor.z, 0.0f,
                      0.0f,     0.0f,     0.0f,     1.0f))
  }

}