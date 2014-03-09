package edu.utexas.cs.sdao.reyes.ui

import javax.swing.JPanel
import java.awt.{Color, Graphics2D, Graphics}
import edu.utexas.cs.sdao.reyes.core.Texture

/**
 * A JPanel subclass that displays a texture
 * @param img the texture to display
 */
class TexturePanel(private var img: Option[Texture] = None) extends JPanel {

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    val g2 = g.asInstanceOf[Graphics2D]

    img match {
      case Some(i) => i.drawInGraphics(g2, this.getWidth, this.getHeight)
      case None =>
        g2.setColor(Color.BLACK)
        g2.fillRect(0, 0, this.getWidth, this.getHeight)
    }
  }

  def image: Option[Texture] = img

  def setImage(newImg: Option[Texture]) = {
    img = newImg
  }

  def setImage(newImg: Texture) = {
    img = Some(newImg)
  }

}
