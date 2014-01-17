package edu.utexas.cs.sdao.reyes.ui

import javax.swing.JPanel
import java.awt.image.BufferedImage
import java.awt.{RenderingHints, Graphics2D, Graphics}

class ImagePanel(img: BufferedImage) extends JPanel {

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2.drawImage(img, 0, 0, this.getWidth, this.getHeight, null)
  }

}
