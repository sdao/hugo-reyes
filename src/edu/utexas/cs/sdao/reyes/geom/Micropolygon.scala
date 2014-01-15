package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{ZBuffer, Color, Vector2, Vector3}
import java.awt.image.BufferedImage

/**
 * An individual micropolygon that has been busted out from a micropolygon grid.
 */
case class Micropolygon(v1: Vector3, v2: Vector3, v3: Vector3, v4: Vector3,
                        normal: Vector3, color: Color) {

  def contains(x: Float, y: Float) = {

  }

  def rasterize(buffer: BufferedImage, zBuffer: ZBuffer) = {
    val x = v1.x.toInt
    val y = v1.y.toInt
    val z = v1.z
    if (x > 0 && y > 0 && x < buffer.getWidth && y < buffer.getHeight) {
      if (zBuffer.tryPaint(x, y, z))
        buffer.setRGB(x, y, color.rgb)
    }
  }

}
