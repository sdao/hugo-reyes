package edu.utexas.cs.sdao.reyes.core

class ZBuffer(width: Int, height: Int) {

  val data = Array.fill(width * height)(Float.NegativeInfinity)

  private def idx(x: Int, y: Int) = x * height + y

  def tryPaint(x: Int, y: Int, z: Float): Boolean = {
    if (z > data(idx(x, y))) { // Note: z-indices are negative; 0 is closest.
      data(idx(x, y)) = z
      true
    } else {
      false
    }
  }

}
