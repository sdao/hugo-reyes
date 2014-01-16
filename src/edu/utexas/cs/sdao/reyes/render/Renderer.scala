package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.geom.{DiceInfo, SplitSurface, Surface}
import scala.collection.mutable
import edu.utexas.cs.sdao.reyes.core.{Vector3, Color, Camera}
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import javax.swing.{SwingUtilities, JFrame}
import java.awt.Dimension
import edu.utexas.cs.sdao.reyes.ui.ImagePanel

object Renderer {

  def split(surfaces: Iterable[Surface], cam: Camera): List[DiceInfo] = {
    val queue = mutable.Queue[SplitSurface]()
    for (x <- surfaces) {
      if (cam.containsBoundingBox(cam.project(x.boundingBox)))
        queue.enqueue(x.toSplitSurface)
    }

    val done = mutable.MutableList[DiceInfo]()
    /* val debugDiscard = mutable.MutableList[DiceInfo]() */

    while (queue.nonEmpty) {
      val surface = queue.dequeue()
      val grid = surface.dice().project(cam)

      if (grid.isVisible) {
        grid.isSplittable match {
          case Some(dir) => queue ++= grid.split(dir)
          case None => done += grid.diceInfo
        }
      } /* else {
        debugDiscard += grid.diceInfo
      } */
    }

    done.toList
  }

  def debugVerifySplit(diceInfos: Iterable[DiceInfo]): Unit =
    debugVerifySplitSurfaces(diceInfos.map(_.surface))

  def debugVerifySplitSurfaces(splitSurfaces: Iterable[SplitSurface]): Unit = {
    splitSurfaces.groupBy(x => x.surface).map(group => {
      println(s"Surface: ${group._1}")

      val sorted = group._2.toVector.sortBy(x => (x.startU, x.startV))
      sorted.map(x => println(s"${x.startU} < u < ${x.endU}; ${x.startV} < v < ${x.endV}"))

      val remain = sorted.foldLeft(0.0)((accum, cur) => accum + (cur.endU - cur.startU) * (cur.endV - cur.startV))
      println(s"Remaining: $remain")
    })
  }

  private def renderSingle(diceInfo: DiceInfo, cam: Camera) = {
    println(s"Rendering $diceInfo")
    val dicedGrid = diceInfo.dice
    val shadedGrid = dicedGrid.shade(DisplacementShaders.bumpyDisplace,
      ColorShaders.checker(ColorShaders.diffuse(Color.GREEN, Vector3(1.0f, -0.5f, 1.0f).normalize),
        ColorShaders.diffuse(Color.BLUE, Vector3(1.0f, -0.5f, 1.0f).normalize)),
      recalcNormals = true)
    val projectedGrid = shadedGrid.project(cam)
    cam.render((buffer, zBuffer) => projectedGrid.rasterize(buffer, zBuffer))
  }

  def render(diceInfos: Iterable[DiceInfo], cam: Camera) {
    diceInfos.par.map(diceInfo => { renderSingle(diceInfo, cam) })
  }

  def renderInteractive(diceInfos: Iterable[DiceInfo], cam: Camera) {
    val frame = new JFrame("Render Output")
    val panel = new ImagePanel(cam.image)

    frame.setPreferredSize(new Dimension(cam.width, cam.height))
    frame.add(panel)
    frame.pack()
    frame.setVisible(true)

    diceInfos.par.map(diceInfo => {
      renderSingle(diceInfo, cam)
      SwingUtilities.invokeAndWait(new Runnable {
        def run(): Unit = panel.repaint()
      })
    })
  }

}
