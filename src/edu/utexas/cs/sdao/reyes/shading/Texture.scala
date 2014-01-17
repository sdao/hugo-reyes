package edu.utexas.cs.sdao.reyes.shading

import math._
import java.awt.image.BufferedImage
import edu.utexas.cs.sdao.reyes.core.{Vector2, Color}

case class Texture(img: BufferedImage) {

  private def getColor(x: Int, y: Int): Color = {
    Color.fromRGB(img.getRGB(x, y))
  }

  /**
   * Samples a pixel from the texture using bilinear filtering.
   * @param uv the UV coordinate to sample
   * @return the sampled color
   */
  def apply(uv: Vector2): Color = {
    val x = uv.x * (img.getWidth - 1)
    val y = uv.y * (img.getHeight - 1)

    val x1 = floor(x).toInt
    val x2 = min(img.getWidth - 1, ceil(x).toInt)

    val y1 = floor(y).toInt
    val y2 = min(img.getHeight - 1, ceil(y).toInt)

    val xTail = x - x1
    val xHead = 1.0f - xTail
    val yTail = y - y1
    val yHead = 1.0f - yTail

    getColor(x1, y1) * xHead * yHead +
      getColor(x2, y1) * xTail * yHead +
      getColor(x1, y2) * xHead * yTail +
      getColor(x2, y2) * xTail * yTail
  }

}
