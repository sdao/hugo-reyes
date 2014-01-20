package edu.utexas.cs.sdao.reyes.ui

import javax.swing.JPanel
import java.awt.{Graphics2D, Graphics}
import edu.utexas.cs.sdao.reyes.core.Texture

class TexturePanel(img: Texture) extends JPanel {

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    val g2 = g.asInstanceOf[Graphics2D]
    img.drawInGraphics(g2, this.getWidth, this.getHeight)
  }

}
