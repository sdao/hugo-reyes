package edu.utexas.cs.sdao.reyes.ui

import javax.swing.JPanel
import java.awt.image.BufferedImage
import java.awt.Graphics

class ImagePanel(img: BufferedImage) extends JPanel {

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    g.drawImage(img, 0, 0, null)
  }

}
