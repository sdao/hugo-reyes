package edu.utexas.cs.sdao.reyes.core

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import scala.math._
import java.awt.{Graphics2D, RenderingHints}

/**
 * A wrapper around Java's BufferedImage that provides texture-specific functionality,
 * such as bilinear filtering and coordinate flipping.
 *
 * @param backingImage the buffered image backing the wrapper
 * @param flipped whether the origin should start at the bottom-left instead of the top-left
 */
class Texture(backingImage: BufferedImage,
              flipped: Boolean = true) extends ColorMap {

  /**
   * The width of the image, in pixels.
   */
  val width = backingImage.getWidth

  /**
   * The height of the image, in pixels.
   */
  val height = backingImage.getHeight

  private def yFunc(y: Int): Int =
    if (flipped)
      height - y - 1
    else
      y

  /**
   * Gets the color at the specified pixel.
   * @param x the pixel X-coordinate
   * @param y the pixel Y-coordinate
   * @return the color at the pixel
   */
  def getColor(x: Int, y: Int): Color = Color.fromRGB(backingImage.getRGB(x, yFunc(y)))

  /**
   * Sets the color at the specified pixel.
   * @param x the pixel X-coordinate
   * @param y the pixel Y-coordinate
   * @param c the color to set at the pixel
   */
  def setColor(x: Int, y: Int, c: Color): Unit = backingImage.setRGB(x, yFunc(y), c.clamp.rgb)

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

  /**
   * Produces a new image with the current image's picture data
   * scaled to the new width and height.
   * Bilinear interpolation will be used in forming the scaled image.
   * @param newWidth the width of the new scaled image
   * @param newHeight the height of the new scaled image
   * @return the new scaled image
   */
  def resize(newWidth: Int, newHeight: Int): Texture = {
    val newImage = new BufferedImage(newWidth, newHeight, backingImage.getType)

    val g2 = newImage.createGraphics()
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2.drawImage(backingImage, 0, 0, newWidth, newHeight, null)
    g2.dispose()

    new Texture(newImage, flipped)
  }

  def resize(newDims: (Int, Int)): Texture = resize(newDims._1, newDims._2)

  /**
   * Writes the image to a PNG file.
   * @param name the name of the image file, preferably ending in ".png"
   * @return whether the write succeeded
   */
  def writeToFile(name: String): Boolean = {
    val f = new File(name)
    f.createNewFile()
    ImageIO.write(backingImage, "png", f)
  }

  /**
   * Draws the image in the specified graphics context.
   * @param g2 the graphics context
   * @param drawWidth the width to draw the image in
   * @param drawHeight the height to draw the image in
   * @return whether the image has been completely drawn, or whether pixels are still changing
   */
  def drawInGraphics(g2: Graphics2D, drawWidth: Int, drawHeight: Int): Boolean = {
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2.drawImage(backingImage, 0, 0, drawWidth, drawHeight, null)
  }

}

object Texture {

  /**
   * Constructs a texture from the image at a specified path.
   * @param path the path of the texture image
   * @param flipped whether the origin should start at the bottom-left instead of the top-left;
   *                most external texture files are not flipped
   * @return the new texture
   */
  def apply(path: String, flipped: Boolean = false) = {
    new Texture(ImageIO.read(new File(path)), flipped)
  }

  /**
   * Constructs an empty RGB texture with the specified width and height.
   * @param width the width of the new texture
   * @param height the height of the new texture
   * @param flipped whether the origin should start at the bottom-left instead of the top-left;
   *                most internal graphics uses rely on flipped coordinates
   * @return the new, empty texture
   */
  def apply(width: Int, height: Int, flipped: Boolean) = {
    new Texture(new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB), flipped)
  }

}
