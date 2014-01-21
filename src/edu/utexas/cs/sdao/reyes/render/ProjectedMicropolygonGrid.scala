package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.core._
import scala.math._
import MathHelpers._
import scala.Some
import edu.utexas.cs.sdao.reyes.geom.{SplitV, SplitU, SplitDirection, SplitSurface}

class ProjectedMicropolygonGrid(width: Int,
                                height: Int,
                                surface: SplitSurface,
                                data: Array[(Vector3, Vector3, Vector2, Color)],
                                proj: Projection)
  extends MicropolygonGrid(width, height, surface, data) {

  // We have to calculate the maximum length along the U- and V-axes.
  private lazy val maxUDist = (0 until height).map(v => {
    (0 until width - 1).map(u => {
      val v1 = getVertex(u, v)
      val v2 = getVertex(u + 1, v)
      v1.dist2D(v2)
    }).sum
  }).max

  private lazy val maxVDist = (0 until width).map(u => {
    (0 until height - 1).map(v => {
      val v1 = getVertex(u, v)
      val v2 = getVertex(u, v + 1)
      v1.dist2D(v2)
    }).sum
  }).max

  /**
   * Determines if the current micropolygon grid can be split and, if
   * so, on which axis.
   * Before calling this function, the grid must be projected into
   * screen space. If not, then the behavior of this function is
   * indeterminate.
   * @return None if the grid isn't splittable;
   *         Some[SplitDirection] if the grid should be split on the
   *         given direction
   */
  def isSplittable: Option[SplitDirection] = {
    if (max(maxUDist, maxVDist) > ProjectedMicropolygonGrid.SPLIT_THRESHOLD &&
        surface.splitCount < ProjectedMicropolygonGrid.MAX_SPLIT) {
      if (maxUDist > maxVDist)
        Some(SplitU)
      else
        Some(SplitV)
    } else {
      None
    }
  }

  /**
   * Helper function to perform a split of the original surface.
   * @param dir the direction to split
   * @return the split surfaces
   */
  def split(dir: SplitDirection) = surface.split(dir)

  /**
   * Helper function to calculate dicing and shading parameters for the original surface
   * for use further down in the pipeline.
   * @return the pipeline info
   */
  def pipelineInfo = PipelineInfo(surface,
      boundingBox,
      limit(1, ProjectedMicropolygonGrid.MAX_DICE, ceil(maxUDist * ProjectedMicropolygonGrid.SAMPLE_RATE).toInt),
      limit(1, ProjectedMicropolygonGrid.MAX_DICE, ceil(maxVDist * ProjectedMicropolygonGrid.SAMPLE_RATE).toInt))

  /**
   * Determines if the current micropolygon grid is visible.
   * This will also clip objects between the camera and the near plane.
   * @return the visibility of the grid
   */
  def isVisible: Boolean = {
    proj.containsScreenBoundingBox(boundingBox)
  }

  /**
   * Rasterizes the projected micropolygon grid into an image buffer and z-buffer
   * by busting it into individual micropolygons.
   * @param buffer the image buffer to rasterize with
   * @param zBuffer the z-buffer to rasterize with
   */
  def rasterize(buffer: Texture, zBuffer: ZBuffer) = {
    // Bust into micropolygons.
    (0 until width - 1).flatMap(u => {
      (0 until height - 1).map(v => {
        val v1 = getVertex(u, v)
        val v2 = getVertex(u + 1, v)
        val v3 = getVertex(u + 1, v + 1)
        val v4 = getVertex(u, v + 1)
        val color = getColor(u, v)
        Micropolygon(v1, v2, v3, v4, color)
      })
    }).foreach(_.rasterize(buffer, zBuffer))
  }

}

object ProjectedMicropolygonGrid {

  /**
   * The maximum pixel size along the U- or V-axes at which splitting should stop.
   */
  val SPLIT_THRESHOLD = 64

  /**
   * The maximum number of splits to perform, regardless of the splitting threshold.
   */
  val MAX_SPLIT = 20

  /**
   * The maximum number of dices to perform, regardless of the U- or V-size.
   */
  val MAX_DICE = 200

  /**
   * About how many micropolygons to generate per pixel-axis.
   * Thus, a value of 2 generates two micropolygons per pixel U-axis and pixel V-axis.
   * This means a total of about 4 micropolygons total per pixel.
   */
  val SAMPLE_RATE = 2.0f
}
