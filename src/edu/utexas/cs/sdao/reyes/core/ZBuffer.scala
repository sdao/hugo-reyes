package edu.utexas.cs.sdao.reyes.core

class ZBuffer(val width: Int, val height: Int) {

  private val data = Array.fill(width * height)(Float.NegativeInfinity)

  private def idx(x: Int, y: Int) = x * height + y

  /**
   * If the specified pixel can be painted at the given z-depth,
   * then the z-depth will be updated and the function returns true.
   *
   * Otherwise, the function returns false.
   *
   * @param x the x-coordinate of the pixel to test
   * @param y the y-coordinate of the pixel to test
   * @param z the z-depth to test
   * @return whether the pixel can be painted at
   */
  def tryPaint(x: Int, y: Int, z: Float): Boolean = {
    if (z > data(idx(x, y))) { // Note: z-indices are negative; 0 is closest.
      data(idx(x, y)) = z
      true
    } else {
      false
    }
  }

  /**
   * Checks if the specified pixel can be painted atop, but does
   * not update the z-depth map.
   *
   * @param x the x-coordinate of the pixel to test
   * @param y the y-coordinate of the pixel to test
   * @param z the z-depth to test
   * @return whether the pixel can be painted at
   */
  def canPaint(x: Int, y: Int, z: Float): Boolean = z > data(idx(x, y))

  /**
   * Copies this z-buffer into another z-buffer.
   * The other z-buffer must have the same width and height.
   *
   * @param otherBuffer the buffer to copy into; its contents will be replaced
   */
  def copyInto(otherBuffer: ZBuffer) = {
    Array.copy(data, 0, otherBuffer.data, 0, width * height)
  }

}
