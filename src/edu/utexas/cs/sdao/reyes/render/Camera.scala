package edu.utexas.cs.sdao.reyes.render

import math._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import java.awt.{Dimension, RenderingHints}
import edu.utexas.cs.sdao.reyes.core._
import edu.utexas.cs.sdao.reyes.core.MathHelpers._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingBox
import edu.utexas.cs.sdao.reyes.core.EmptyBoundingBox
import edu.utexas.cs.sdao.reyes.geom.{SplitSurface, Surface}
import scala.collection.mutable
import javax.swing.{SwingUtilities, JFrame}
import edu.utexas.cs.sdao.reyes.ui.ImagePanel

/**
 * A pinhole camera projection.
 * The default parameters create a camera pointing along the negative Z-axis.
 * @param worldToCamera a transformation matrix that converts world coordinates
 *                      to camera coordinates; e.g., if the camera is translated one unit
 *                      to the left, then this matrix will move world objects one unit to
 *                      the right
 * @param fieldOfView the camera's horizontal field of view in radians,
 *                    measured as the angle from the left of the visible screen to the right;
 *                    acceptable values are 0 < fieldOfView < Pi
 * @param width the width of the output image plane
 * @param height the height of the output image plane
 */
class Camera(worldToCamera: Matrix4 = Matrix4.IDENTITY,
             fieldOfView: Float = toRadians(60.0).toFloat,
             width: Int = 800,
             height: Int = 600) {

  if (fieldOfView <= 0.0 || fieldOfView >= Pi)
    throw  new IllegalArgumentException("fieldOfView out of range")

  val aspect = width.toFloat/height.toFloat

  protected lazy val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  lazy val zBuffer = new ZBuffer(width, height)

  private val lock : AnyRef = new Object()

  val focalLength = (width / 2.0f) / tan(fieldOfView / 2.0f).toFloat // Trigonometry, what's that?!
  val halfWidth = width / 2.0f
  val halfHeight = height / 2.0f

  /**
   * Projects a point into the image space defined by u=0..width and v=0..height.
   * The z-component is the z-depth of the point.
   * @param u the vector to project
   * @return the projected vector
   */
  def project(u: Vector3): Vector3 = {
    val v = worldToCamera * u
    if (v.z == 0.0f)
      Vector3(focalLength * v.x / Float.MinPositiveValue + halfWidth,
        focalLength * v.y / Float.MinPositiveValue + halfHeight,
        v.z)
    else
      Vector3(focalLength * v.x / -v.z + halfWidth, // Note: negate z because we are using a right-handed system,
        focalLength * v.y / -v.z + halfHeight,      // where the camera points to -z.
        v.z)
  }

  /**
   * Projects the points of a bounding box onto the image space defined by u=0..width and
   * v=0..height.
   * @param b the bounding box to project
   * @return the projected bounding box
   */
  def project(b: FilledBoundingBox): FilledBoundingBox = {
    val newLow = project(b.lowBound)
    val newUp = project(b.upBound)
    BoundingBox.empty
      .expand(newLow)
      .expand(newUp)
  }

  /**
   * Determines if a certain projected bounding box is contained within the camera's
   * view frustum, at least partially.
   * The projected bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be in camera space.
   * This will also clip objects between the camera and the near plane.
   * @param boundingBox the bounding box to check
   * @return the visibility of the bounding box
   */
  def containsBoundingBox(boundingBox: BoundingBox): Boolean = {
    boundingBox match {
      case EmptyBoundingBox() => false
      case FilledBoundingBox(lowBound, upBound) =>
        upBound.x >= 0.0f &&
          lowBound.x <= width &&
          upBound.y >= 0.0f &&
          lowBound.y <= height &&
          lowBound.z < 0.0f
    }
  }

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
      case EmptyBoundingBox() => true
      case FilledBoundingBox(lowBound, upBound) =>
        val zDepth = upBound.z

        val minX = limit(0, width - 1, floor(lowBound.x).toInt)
        val maxX = limit(0, width - 1, ceil(upBound.x).toInt)
        val minY = limit(0, height - 1, floor(lowBound.y).toInt)
        val maxY = limit(0, height - 1, ceil(upBound.y).toInt)

        // We're going to do this the traditional, mutable way for performance.
        var occluded = true
        var x = minX
        while (x <= maxX && occluded) {
          var y = minY
          while (y <= maxY && occluded) {
            occluded = !zBuffer.canPaint(x, buffer.getHeight - y - 1, zDepth)
            y += 1
          }
          x += 1
        }

        occluded
    }
  }

  /**
   * Returns the image buffer as-is.
   * If the camera is supersampling, the image buffer's dimensions will be
   * scaled up by the supersampling factor.
   * @return the image buffer
   */
  def image: BufferedImage = buffer

  def imageDimensions: (Int, Int) = (width, height)

  /**
   * Writes the real-sized image to a PNG file.
   * @param name the name of the image file, preferably ending in ".png"
   * @return whether the write succeeded
   */
  def writeImageFile(name: String) = {
    val f = new File(name)
    f.createNewFile()
    ImageIO.write(image, "png", f)
  }

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
      if (containsBoundingBox(project(x.boundingBox)))
        queue.enqueue(x.toSplitSurface)
    }

    val done = mutable.MutableList[PipelineInfo]()
    /* val debugDiscard = mutable.MutableList[DiceInfo]() */

    while (queue.nonEmpty) {
      val surface = queue.dequeue()
      // We should displace here to more accurately determine how many
      // micropolygons are needed for the displaced surface.
      val grid = surface.dice().shade(displaceOnly = true).project(this)

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
      val shadedGrid = dicedGrid.shade(displaceOnly = displaceOnly)
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
    val panel = new ImagePanel(buffer)

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

object Camera {
  val PROJECT_OFFSET = Vector2(1.0f, 1.0f)
}