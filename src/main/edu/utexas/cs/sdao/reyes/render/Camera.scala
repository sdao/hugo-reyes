package edu.utexas.cs.sdao.reyes.render

import math._
import java.awt.Dimension
import edu.utexas.cs.sdao.reyes.core._
import edu.utexas.cs.sdao.reyes.core.MathHelpers._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingBox
import edu.utexas.cs.sdao.reyes.core.EmptyBoundingBox
import edu.utexas.cs.sdao.reyes.geom.{SplitSurface, Surface}
import scala.collection.mutable
import javax.swing.{SwingUtilities, JFrame}
import edu.utexas.cs.sdao.reyes.ui.TexturePanel
import edu.utexas.cs.sdao.reyes.graph.SurfaceNode

/**
 * A pinhole camera projection.
 * The default parameters create a camera pointing along the negative Z-axis.
 *
 * @param cameraTransform a transformation matrix that represents how the camera
 *                        has been transformed in the world space
 * @param fieldOfView the camera's horizontal field of view in radians,
 *                    measured as the angle from the left of the visible screen to the right;
 *                    acceptable values are 0 < fieldOfView < Pi
 * @param width the width of the output image plane
 * @param height the height of the output image plane
 */
class Camera(cameraTransform: Matrix4 = Matrix4.IDENTITY,
             fieldOfView: Float = toRadians(60.0).toFloat,
             width: Int = 800,
             height: Int = 600)
  extends Projection(cameraTransform, fieldOfView, width, height) {

  protected lazy val buffer = Texture(width, height, flipped = true)
  protected lazy val zBuffer = new ZBuffer(width, height)

  private val lock : AnyRef = new Object()

  /**
   * Estimates whether the z-buffer occludes a projected bounding box.
   * The projected bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be in camera space.
   *
   * @param boundingBox the bounding box to check
   * @return whether the bounding box is occluded
   */
  protected def estimateZBufferOcclusion(boundingBox: BoundingBox): Boolean = {
    boundingBox match {
      case EmptyBoundingBox => true
      case FilledBoundingBox(lowBound, upBound) =>
        val zDepth = upBound.z

        val minX = clamp(0, width - 1, floor(lowBound.x).toInt)
        val maxX = clamp(0, width - 1, ceil(upBound.x).toInt)
        val minY = clamp(0, height - 1, floor(lowBound.y).toInt)
        val maxY = clamp(0, height - 1, ceil(upBound.y).toInt)

        // We're going to do this the traditional, mutable way for performance.
        var occluded = true
        var x = minX
        while (x <= maxX && occluded) {
          var y = minY
          while (y <= maxY && occluded) {
            occluded = !zBuffer.canPaint(x, y, zDepth)
            y += 1
          }
          x += 1
        }

        occluded
    }
  }

  /**
   * Returns a copy of the image buffer.
   * If this function is overridden by a subclass, then imageDimensions must
   * also be overridden.
   * @return a copy of the image buffer
   */
  def image: Texture = buffer

  /**
   * The dimensions, in pixel width and height, of the image returned by
   * the function image.
   * @return
   */
  def imageDimensions: (Int, Int) = (width, height)

  /**
   * Copies the camera's internal z-buffer into an external buffer.
   * The other buffer must have a width and height identical to the
   * camera's width and height.
   *
   * @param otherBuffer the buffer to copy into; its contents will be replaced
   */
  def copyZBuffer(otherBuffer: ZBuffer): Unit = {
    zBuffer.copyInto(otherBuffer)
  }

  /**
   * Writes the real-sized image to a PNG file.
   * @param name the name of the image file, preferably ending in ".png"
   * @return whether the write succeeded
   */
  def writeImageFile(name: String) = image.writeToFile(name)

  /**
   * Splits primitive surfaces into smaller subsurfaces until they clear
   * the size threshold when projected.
   * The result is that objects that are closer to the camera will be
   * split into more partitions.
   * @param surfaces the surfaces to split
   * @return a list containing the split surfaces, prepared for the dicing step
   *         and sorted by increasing z-depth
   */
  def split(surfaces: Iterable[Surface]): Vector[PipelineInfo] = {
    val queue = mutable.Queue[SplitSurface]()
    for (x <- surfaces) {
      // Only enqueue objects that are in the camera's view frustum.
      if (containsScreenBoundingBox(x.boundingSphere.project(this)))
        queue.enqueue(x.toSplitSurface)
    }

    val done = mutable.MutableList[PipelineInfo]()
    /* val debugDiscard = mutable.MutableList[DiceInfo]() */

    while (queue.nonEmpty) {
      val surface = queue.dequeue()
      // We should displace here to more accurately determine how many
      // micropolygons are needed for the displaced surface.
      val grid = surface.dice().shade(this, displaceOnly = true).project(this)

      if (grid.isVisible) { // Only continue with microgrids that we'll actually see.
        grid.isSplittable match {
          case Some(dir) => queue ++= grid.split(dir)
          case None => done += grid.pipelineInfo
        }
      } /* else {
        debugDiscard += grid.pipelineInfo
      } */
    }

    done.toVector.sortBy(-_.zDepth)
  }

  def split(sceneRoot: SurfaceNode): Vector[PipelineInfo] = {
    val nodeQueue = mutable.Queue[SurfaceNode]()
    nodeQueue.enqueue(sceneRoot)

    val surfaces = mutable.MutableList[Surface]()

    // Traverse scene hierarchy (BFS) and transform all nodes.
    while (nodeQueue.nonEmpty) {
      val node = nodeQueue.dequeue()
      if (containsScreenBoundingBox(node.boundingSphere.project(this))) {
        // Add current object to list.
        surfaces += node.transformedSurface

        // Traverse my children!
        nodeQueue ++= node.transformedChildren
      }
    }

    split(surfaces)
  }

  /**
   * Renders a single split surface based on the dicing information given.
   * This function will dice, shade, and rasterize the split surface
   * using the given camera.
   * @param pipelineObj the pipeline object, including the surface to dice
   * @param displaceOnly whether to only run displacement shaders in the shading step
   *                     without running any color shaders
   */
  private def renderSingle(pipelineObj: PipelineInfo,
                           displaceOnly: Boolean = false): Unit = {
    if (!estimateZBufferOcclusion(pipelineObj.boundingBox)) {
      println(s"Rendering $pipelineObj")
      val dicedGrid = pipelineObj.dice
      val shadedGrid = dicedGrid.shade(this, displaceOnly = displaceOnly)
      val projectedGrid = shadedGrid.project(this)
      lock.synchronized {
        projectedGrid.rasterize(buffer, zBuffer)
      }
    } else {
      println(s"Skipping $pipelineObj")
    }
  }

  /**
   * Renders all of the surfaces in a list.
   * This function will split, dice, shade, and rasterize the split surfaces
   * using the given camera.
   * @param surfaces the surfaces to render
   * @param displaceOnly whether to only run displacement shaders in the shading step
   *                     without running any color shaders
   */
  def render(surfaces: Iterable[Surface],
             displaceOnly: Boolean = false) = {
    val pipelineObjs = split(surfaces)
    pipelineObjs.par.foreach(pipelineInfo => { renderSingle(pipelineInfo, displaceOnly) })
    println("Render complete.")
  }

  /**
   * Renders all of the split surfaces in a list, displaying an
   * image preview window as the rendering occurs.
   * This function will split, dice, shade, and rasterize the split surfaces
   * using the given camera.
   * @param surfaces the surfaces to render
   */
  def renderInteractive(surfaces: Iterable[Surface]) {
    val frame = new JFrame("Render Output")
    val panel = new TexturePanel(buffer)

    frame.setPreferredSize(new Dimension(imageDimensions._1, imageDimensions._2))
    frame.add(panel)
    frame.pack()
    frame.setVisible(true)

    val pipelineObjs = split(surfaces)
    pipelineObjs.par.foreach(pipelineInfo => {
      renderSingle(pipelineInfo)
      SwingUtilities.invokeAndWait(new Runnable {
        def run(): Unit = panel.repaint()
      })
    })

    println("Render complete.")
  }

}
