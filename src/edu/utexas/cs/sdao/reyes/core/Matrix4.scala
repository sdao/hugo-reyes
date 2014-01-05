package edu.utexas.cs.sdao.reyes.core

import scala.math._

/**
 * Defines a 4x4 matrix.
 */
class Matrix4(val data: Array[Array[Float]] = Array.ofDim[Float](4, 4)) {

  if (data.length != 4)
    throw new IllegalArgumentException("Dim of rows != 4")
  if (data(0).length != 4 || data(1).length != 4 || data(2).length != 4 || data(3).length != 4)
    throw new IllegalArgumentException("Dim of columns != 4")

  private def mult(arr1: Array[Array[Float]], arr2: Array[Array[Float]]) = {
    val newData = Array.ofDim[Float](4, 4)

    for (i <- 0 until 4) { // i = row
      for (j <- 0 until 4) { // j = col
        newData(i)(j) =
          arr1(i)(0) * arr2(0)(j)
          + arr1(i)(1) * arr2(1)(j)
          + arr1(i)(2) * arr2(2)(j)
          + arr1(i)(3) * arr2(3)(j)
      }
    }

    newData
  }

  def *(right: Matrix4) = new Matrix4(mult(data, right.data))

  def rotateX(rads: Float) = new Matrix4(mult(data, Matrix4.rotationX(rads).data))

  def rotateY(rads: Float) = new Matrix4(mult(data, Matrix4.rotationY(rads).data))

  def rotateZ(rads: Float) = new Matrix4(mult(data, Matrix4.rotationZ(rads).data))

  def translate(t: Vector3) = new Matrix4(mult(data, Matrix4.translation(t).data))

  def scale(s: Vector3) = new Matrix4(mult(data, Matrix4.scaling(s).data))

  def apply(x: Int, y: Int) = data(x)(y)

}

object Matrix4 {

  val IDENTITY = new Matrix4(Array(Array(1.0f, 0.0f, 0.0f, 0.0f),
                                   Array(0.0f, 1.0f, 0.0f, 0.0f),
                                   Array(0.0f, 0.0f, 1.0f, 0.0f),
                                   Array(0.0f, 0.0f, 0.0f, 1.0f)))

  val ZERO = new Matrix4(Array(Array(0.0f, 0.0f, 0.0f, 0.0f),
                               Array(0.0f, 0.0f, 0.0f, 0.0f),
                               Array(0.0f, 0.0f, 0.0f, 0.0f),
                               Array(0.0f, 0.0f, 0.0f, 0.0f)))

  def rotationX(rads: Float) = {
    val cos = cos(rads): Float
    val sin = sin(rads): Float

    new Matrix4(Array(Array(1.0f, 0.0f, 0.0f, 0.0f),
                      Array(0.0f, cos,  -sin, 0.0f),
                      Array(0.0f, sin,  cos,  0.0f),
                      Array(0.0f, 0.0f, 0.0f, 1.0f)))
  }

  def rotationY(rads: Float) = {
    val cos = cos(rads): Float
    val sin = sin(rads): Float

    new Matrix4(Array(Array(cos,  0.0f, sin,  0.0f),
                      Array(0.0f, 1.0f, 0.0f, 0.0f),
                      Array(-sin, 0.0f, cos,  0.0f),
                      Array(0.0f, 0.0f, 0.0f, 1.0f)))
  }

  def rotationZ(rads: Float) = {
    val cos = cos(rads): Float
    val sin = sin(rads): Float

    new Matrix4(Array(Array(cos,  -sin, 0.0f, 0.0f),
                      Array(sin,  cos,  0.0f, 0.0f),
                      Array(0.0f, 0.0f, 1.0f, 0.0f),
                      Array(0.0f, 0.0f, 0.0f, 1.0f)))
  }

  def translation(amount: Vector3) = {
    new Matrix4(Array(Array(1.0f, 0.0f, 0.0f, amount.x),
                      Array(0.0f, 1.0f, 0.0f, amount.y),
                      Array(0.0f, 0.0f, 1.0f, amount.z),
                      Array(0.0f, 0.0f, 0.0f, 1.0f)))
  }

  def scaling(factor: Vector3) = {
    new Matrix4(Array(Array(factor.x, 0.0f, 0.0f, 0.0f),
                      Array(0.0f, factor.y, 0.0f, 0.0f),
                      Array(0.0f, 0.0f, factor.z, 0.0f),
                      Array(0.0f, 0.0f, 0.0f, 1.0f)))
  }
}