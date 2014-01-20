package edu.utexas.cs.sdao.reyes.core

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import scala.math._
import java.awt.{Graphics2D, RenderingHints}

/**
 * A wrapper around Java's BufferedImage that uses the bottom-left corner as the origin,
 * with positive x-coordinates going to the right and positive y-coordinates to the top.
 *
 * @param backingImage the buffered image backing the wrapper
 */
class Texture(backingImage: BufferedImage) {

  val width = backingImage.getWidth
  val height = backingImage.getHeight

  def getColor(x: Int, y: Int): Color = Color.fromRGB(backingImage.getRGB(x, height - y - 1))

  def setColor(x: Int, y: Int, c: Color): Unit = backingImage.setRGB(x, height - y - 1, c.clamp.rgb)

  /**
   * Samples a pixel from the texture using bilinear filtering.
   * @param uv the UV coordinate to sample; the coordinates must be in the range [0, 1]
   * @return the sampled color
   */
  def sampleColor(uv: Vector2): Color = {
    val x = uv.x * (width - 1)
    val y = uv.y * (height - 1)

    val x1 = floor(x).toInt
    val x2 = ceil(x).toInt

    val y1 = floor(y).toInt
    val y2 = ceil(y).toInt

    val xTail = x - x1
    val xHead = 1.0f - xTail
    val yTail = y - y1
    val yHead = 1.0f - yTail

    getColor(x1, y1) * xHead * yHead +
      getColor(x2, y1) * xTail * yHead +
      getColor(x1, y2) * xHead * yTail +
      getColor(x2, y2) * xTail * yTail
  }

  def scale(newWidth: Int, newHeight: Int): Texture = {
    val newImage = new BufferedImage(newWidth, newHeight, backingImage.getType)

    val g2 = newImage.createGraphics()
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2.drawImage(backingImage, 0, 0, newWidth, newHeight, null)
    g2.dispose()

    new Texture(newImage)
  }

  /**
   * Writes the real-sized image to a PNG file.
   * @param name the name of the image file, preferably ending in ".png"
   * @return whether the write succeeded
   */
  def writeToFile(name: String): Boolean = {
    val f = new File(name)
    f.createNewFile()
    ImageIO.write(backingImage, "png", f)
  }

  def drawInGraphics(g2: Graphics2D, drawWidth: Int, drawHeight: Int) = {
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2.drawImage(backingImage, 0, 0, drawWidth, drawHeight, null)
  }

}

object Texture {

  def apply(path: String) = {
    new Texture(ImageIO.read(new File(path)))
  }

  def apply(width: Int, height: Int) = {
    new Texture(new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB))
  }

}
