package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.core._
import math._

/**
 * An individual micropolygon that has been busted out from a micropolygon grid.
 */
case class Micropolygon(v1: Vector3, v2: Vector3, v3: Vector3, v4: Vector3,
                        color: Color) {

  def boundingBox = {
    BoundingBox.empty.expand(v1).expand(v2).expand(v3).expand(v4)
  }

  /**
   * Determines whether the micropolygon contains the given screen point.
   * Note that the current implementation returns false for points contained
   * within a micropolygon whose normal is pointing away from the camera,
   * i.e. one that is backfacing.
   * @param v the screen point to test
   * @return whether the screen point is contained in the micrpolygon
   */
  def contains(v: Vector2) = {
    ((v.y - v1.y) * (v1.x - v2.x) - (v.x - v1.x) * (v1.y - v2.y)) >= 0 &&
      ((v.y - v2.y) * (v2.x - v3.x) - (v.x - v2.x) * (v2.y - v3.y)) >= 0 &&
      ((v.y - v3.y) * (v3.x - v4.x) - (v.x - v3.x) * (v3.y - v4.y)) >= 0 &&
      ((v.y - v4.y) * (v4.x - v1.x) - (v.x - v4.x) * (v4.y - v1.y)) >= 0
  }

  /**
   * Paints the micropolygon onto a buffer, using a z-buffer to determine overlaps.
   * Points outside of a near-far range will be culled, as will points off the
   * output image rectangle.
   * @param buffer the buffer in which to paint the image
   * @param zBuffer the z-buffer to use in calculating overlaps
   */
  def rasterize(buffer: Texture, zBuffer: ZBuffer) = {
    val bounds = boundingBox
    for (x <- floor(bounds.lowBound.x).toInt to ceil(bounds.upBound.x).toInt) {
      for (y <- floor(bounds.lowBound.y).toInt to ceil(bounds.upBound.y).toInt) {
        if (x >= 0 && x < buffer.width &&
          y >= 0 && y < buffer.height &&
          v1.z < 0.0f) {
          if (contains(Vector2(x, y))) {
            if (zBuffer.tryPaint(x, y, v1.z))
              buffer.setColor(x, y, color)
          }
        }
      }
    }

  }

}
