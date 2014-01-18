package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.geom.{SplitSurface, Surface}
import scala.collection.mutable
import edu.utexas.cs.sdao.reyes.core.{FilledBoundingBox, Vector3, Color, Camera}
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import javax.swing.{SwingUtilities, JFrame}
import java.awt.Dimension
import edu.utexas.cs.sdao.reyes.ui.ImagePanel
import math._

/**
 * Contains functions for splitting and rasterizing objects using the
 * Reyes algorithm.
 */
object Renderer {

  val BUCKET_SIZE = 48

  /**
   * Splits primitive surfaces into smaller subsurfaces until they clear
   * the size threshold when projected.
   * The result is that objects that are closer to the camera will be
   * split into more partitions.
   * @param surfaces the surfaces to split
   * @param cam the camera to project the surfaces onto when splitting.
   * @return a list containing the split surfaces, prepared for the dicing step
   */
  def split(surfaces: Iterable[Surface], cam: Camera): List[PipelineInfo] = {
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
          case None => done += grid.diceInfo
        }
      } /* else {
        debugDiscard += grid.diceInfo
      } */
    }

    done.toList
  }

  def debugVerifySplit(diceInfos: Iterable[PipelineInfo]): Unit =
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

  def bucket(pipelineInfo: Iterable[PipelineInfo], cam: Camera): Iterable[Bucket] = {
    val numBucketsX = ceil(cam.width.toFloat / BUCKET_SIZE).toInt
    val numBucketsY = ceil(cam.height.toFloat / BUCKET_SIZE).toInt

    (0 until numBucketsX).flatMap(u => {
      (0 until numBucketsY).map(v => {
        val xMin = u * BUCKET_SIZE
        val yMin = v * BUCKET_SIZE
        val xMax = min(cam.width, xMin + BUCKET_SIZE)
        val yMax = min(cam.height, yMin + BUCKET_SIZE)
        val bounds = FilledBoundingBox(Vector3(xMin, yMin, 0), Vector3(xMax, yMax, 0))

        val objects = pipelineInfo.filter(info => {
          bounds.intersects2D(info.boundingBox)
        })
        
        Bucket(xMin, yMin, xMax, yMax, objects, cam)
      })
    }).toIterable
  }

  /**
   * Renders all of the split surfaces in a list.
   * This function will dice, shade, and rasterize the split surfaces
   * using the given camera.
   * @param objects the pipeline objects to render
   * @param cam the camera on which to project
   */
  def render(objects: Iterable[PipelineInfo], cam: Camera): Unit = {
    Renderer.bucket(objects, cam).par.filter(_.objects.nonEmpty).map(x => {
      x.render()
      println("Rendered bucket")
    })
  }

  /**
   * Renders all of the split surfaces in a list, displaying an
   * image preview window as the rendering occurs.
   * This function will dice, shade, and rasterize the split surfaces
   * using the given camera.
   * @param objects the pipeline objects to render
   * @param cam the camera on which to project
   */
  def renderInteractive(objects: Iterable[PipelineInfo], cam: Camera): Unit = {
    val frame = new JFrame("Render Output")
    val panel = new ImagePanel(cam.image)

    frame.setPreferredSize(new Dimension(cam.realWidth, cam.realHeight))
    frame.add(panel)
    frame.pack()
    frame.setVisible(true)

    Renderer.bucket(objects, cam).par.filter(_.objects.nonEmpty).map(x => {
      x.render()
      SwingUtilities.invokeAndWait(new Runnable {
        def run(): Unit = panel.repaint()
      })
    })
  }

}
