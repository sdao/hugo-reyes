package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core._
import java.awt.image.BufferedImage
import math._

/**
 * An individual micropolygon that has been busted out from a micropolygon grid.
 */
case class Micropolygon(v1: Vector3, v2: Vector3, v3: Vector3, v4: Vector3,
                        normal: Vector3, color: Color) {

  def boundingBox = {
    BoundingBox.empty.expand(v1).expand(v2).expand(v3).expand(v4)
  }

  def contains(v: Vector2) = {
    ((v.y - v1.y) * (v1.x - v2.x) - (v.x - v1.x) * (v1.y - v2.y)) <= 0 &&
      ((v.y - v2.y) * (v2.x - v3.x) - (v.x - v2.x) * (v2.y - v3.y)) <= 0 &&
      ((v.y - v3.y) * (v3.x - v4.x) - (v.x - v3.x) * (v3.y - v4.y)) <= 0 &&
      ((v.y - v4.y) * (v4.x - v1.x) - (v.x - v4.x) * (v4.y - v1.y)) <= 0
  }

  def rasterize(buffer: BufferedImage, zBuffer: ZBuffer) = {
    val bounds = boundingBox
    for (x <- bounds.lowBound.x to bounds.upBound.x by 1.0f) {
      for (y <- bounds.lowBound.y to bounds.upBound.y by 1.0f) {
        if (contains(Vector2(x, y))) {
          val xx = max(min(buffer.getWidth, x), 0).toInt
          val yy = max(min(buffer.getWidth, y), 0).toInt

          if (zBuffer.tryPaint(xx, yy, v1.z))
            buffer.setRGB(xx, yy, color.clamp.rgb)
        }
      }
    }

  }

}
