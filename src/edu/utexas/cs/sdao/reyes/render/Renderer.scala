package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.geom.{SplitSurface, Surface}
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
   *         and sorted by increasing z-depth
   */
  def split(surfaces: Iterable[Surface], cam: Camera): Vector[PipelineInfo] = {
    val queue = mutable.Queue[SplitSurface]()
    for (x <- surfaces) {
      // Only enqueue objects that are in the camera's view frustum.
      if (cam.containsBoundingBox(cam.project(x.boundingBox)))
        queue.enqueue(x.toSplitSurface)
    }

    val done = mutable.MutableList[PipelineInfo]()
    /* val debugDiscard = mutable.MutableList[DiceInfo]() */

    while (queue.nonEmpty) {
      val surface = queue.dequeue()
      // We should displace here to more accurately determine how many
      // micropolygons are needed for the displaced surface.
      val grid = surface.dice().shade(displaceOnly = true).project(cam)

      if (grid.isVisible) { // Only continue with microgrids that we'll actually see.
        grid.isSplittable match {
          case Some(dir) => queue ++= grid.split(dir)
          case None => done += grid.pipelineInfo
        }
      } /* else {
        debugDiscard += grid.diceInfo
      } */
    }

    done.toVector.sortBy(-_.zDepth)
  }

  def debugVerifySplit(pipelineObjs: Iterable[PipelineInfo]): Unit =
    debugVerifySplitSurfaces(pipelineObjs.map(_.surface))

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
   * @param diceInfo the pipeline object, including the surface to dice
   * @param cam the camera on which to project
   */
  private def renderSingle(diceInfo: PipelineInfo, cam: Camera): Unit = {
    println(s"Rendering $diceInfo")
    val dicedGrid = diceInfo.dice
    val shadedGrid = dicedGrid.shade()
    val projectedGrid = shadedGrid.project(cam)
    cam.render((buffer, zBuffer) => projectedGrid.rasterize(buffer, zBuffer))
  }

  /**
   * Renders all of the split surfaces in a list.
   * This function will dice, shade, and rasterize the split surfaces
   * using the given camera.
   * @param pipelineObjs the pipeline objects, including the surface to dice
   * @param cam the camera on which to project
   */
  def render(pipelineObjs: Iterable[PipelineInfo], cam: Camera) = {
    pipelineObjs.par.foreach(pipelineInfo => { renderSingle(pipelineInfo, cam) })
    println("Render complete.")
  }

  /**
   * Like render(), but processes each diced chunk sequentially.
   * You should probably use render() instead.
   * @param pipelineObjs the pipeline objects, including the surface to dice
   * @param cam the camera on which to project
   */
  def renderSequential(pipelineObjs: Iterable[PipelineInfo], cam: Camera) = {
    pipelineObjs.foreach(pipelineInfo => { renderSingle(pipelineInfo, cam) })
    println("Render complete.")
  }

  /**
   * Renders all of the split surfaces in a list, displaying an
   * image preview window as the rendering occurs.
   * This function will dice, shade, and rasterize the split surfaces
   * using the given camera.
   * @param pipelineObjs the pipeline objects, including the surface to dice
   * @param cam the camera on which to project
   */
  def renderInteractive(pipelineObjs: Iterable[PipelineInfo], cam: Camera) {
    val frame = new JFrame("Render Output")
    val panel = new ImagePanel(cam.image)

    frame.setPreferredSize(new Dimension(cam.realWidth, cam.realHeight))
    frame.add(panel)
    frame.pack()
    frame.setVisible(true)

    pipelineObjs.par.foreach(pipelineInfo => {
      renderSingle(pipelineInfo, cam)
      SwingUtilities.invokeAndWait(new Runnable {
        def run(): Unit = panel.repaint()
      })
    })

    println("Render complete.")
  }

}
