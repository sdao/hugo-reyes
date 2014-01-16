package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.geom.{DiceInfo, SplitSurface, Surface}
import scala.collection.mutable
import edu.utexas.cs.sdao.reyes.core.{Vector3, Color, Camera}
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import javax.swing.{SwingUtilities, JFrame}
import java.awt.Dimension
import edu.utexas.cs.sdao.reyes.ui.ImagePanel

/**
 * Contains functions for splitting and rasterizing objects using the
 * Reyes algorithm.
 */
object Renderer {

  /**
   * Splits primitive surfaces into smaller subsurfaces until they clear
   * the size threshold when projected.
   * The result is that objects that are closer to the camera will be
   * split into more partitions.
   * @param surfaces the surfaces to split
   * @param cam the camera to project the surfaces onto when splitting.
   * @return a list containing the split surfaces, prepared for the dicing step
   */
  def split(surfaces: Iterable[Surface], cam: Camera): List[DiceInfo] = {
    val queue = mutable.Queue[SplitSurface]()
    for (x <- surfaces) {
      // Only enqueue objects that are in the camera's view frustum.
      if (cam.containsBoundingBox(cam.project(x.boundingBox)))
        queue.enqueue(x.toSplitSurface)
    }

    val done = mutable.MutableList[DiceInfo]()
    /* val debugDiscard = mutable.MutableList[DiceInfo]() */

    while (queue.nonEmpty) {
      val surface = queue.dequeue()
      val grid = surface.dice().project(cam)

      if (grid.isVisible) { // Only continue with microgrids that we'll actually see.
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

  /**
   * Renders a single split surface based on the dicing information given.
   * This function will dice, shade, and rasterize the split surface
   * using the given camera.
   * @param diceInfo the dicing parameters, including the surface to dice
   * @param cam the camera on which to project
   */
  private def renderSingle(diceInfo: DiceInfo, cam: Camera): Unit = {
    println(s"Rendering $diceInfo")
    val dicedGrid = diceInfo.dice
    val shadedGrid = dicedGrid.shade(DisplacementShaders.bumpyDisplace,
      ColorShaders.checker(ColorShaders.diffuse(Color.GREEN, Vector3(0.0f, 0.5f, 1.0f).normalize),
        ColorShaders.diffuse(Color.BLUE, Vector3(0.0f, 0.5f, 1.0f).normalize)),
      recalcNormals = true)
    val projectedGrid = shadedGrid.project(cam)
    cam.render((buffer, zBuffer) => projectedGrid.rasterize(buffer, zBuffer))
  }

  /**
   * Renders all of the split surfaces in a list.
   * This function will dice, shade, and rasterize the split surfaces
   * using the given camera.
   * @param diceInfos the dicing parameters, including the surface to dice
   * @param cam the camera on which to project
   */
  def render(diceInfos: Iterable[DiceInfo], cam: Camera) {
    diceInfos.par.map(diceInfo => { renderSingle(diceInfo, cam) })
  }

  /**
   * Renders all of the split surfaces in a list, displaying an
   * image preview window as the rendering occurs.
   * This function will dice, shade, and rasterize the split surfaces
   * using the given camera.
   * @param diceInfos the dicing parameters, including the surface to dice
   * @param cam the camera on which to project
   */
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
