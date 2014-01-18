package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.core._
import scala.math._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingBox
import edu.utexas.cs.sdao.reyes.core.EmptyBoundingBox
import scala.Some
import java.awt.image.BufferedImage
import edu.utexas.cs.sdao.reyes.geom.{SplitV, SplitU, SplitDirection, SplitSurface}

class ProjectedMicropolygonGrid(width: Int,
                                height: Int,
                                surface: SplitSurface,
                                data: Array[(Vector3, Vector3, Vector2, Color)],
                                cam: Camera)
  extends MicropolygonGrid(width, height, surface, data) {

  lazy val maxUDist = (0 until height).map(v => {
    (0 until width - 1).map(u => {
      val v1 = getVertex(u, v).truncateToVector2
      val v2 = getVertex(u + 1, v).truncateToVector2
      v1.dist(v2)
    }).sum
  }).max

  lazy val maxVDist = (0 until width).map(u => {
    (0 until height - 1).map(v => {
      val v1 = getVertex(u, v).truncateToVector2
      val v2 = getVertex(u, v + 1).truncateToVector2
      v1.dist(v2)
    }).sum
  }).max

  lazy val busted = (0 until width - 1).flatMap(u => {
    (0 until height - 1).map(v => {
      val v1 = getVertex(u, v)
      val v2 = getVertex(u + 1, v)
      val v3 = getVertex(u + 1, v + 1)
      val v4 = getVertex(u, v + 1)
      val norm = getNormal(u, v)
      val color = getColor(u, v)
      Micropolygon(v1, v2, v3, v4, norm, color)
    })
  })

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
   * Helper function to calculate dicing parameters for the original surface.
   * @return the dice info
   */
  def diceInfo = PipelineInfo(surface,
    max(1, ceil(maxUDist * ProjectedMicropolygonGrid.SAMPLE_RATE).toInt),
    max(1, ceil(maxVDist * ProjectedMicropolygonGrid.SAMPLE_RATE).toInt),
    boundingBox)

  /**
   * Determines if the current micropolygon grid is visible.
   * This will also clip objects between the camera and the near plane.
   * @return the visibility of the grid
   */
  def isVisible: Boolean = {
    cam.containsBoundingBox(boundingBox)
  }

  def rasterize(buffer: BufferedImage, zBuffer: ZBuffer) = {
    // Bust into micropolygons.
    busted.map(_.rasterize(buffer, zBuffer, cam.near, cam.far))
  }

  def contains(vec: Vector2): Option[(Float, Color)] = {
    if (boundingBox.contains2D(vec)) {
      var found = false
      var color: Option[Color] = None
      var dist: Float = Float.NegativeInfinity

      var u = 0
      while (u < width - 1 && !found) {
        var v = 0
        while (v < height - 1 && !found) {
          val v1 = getVertex(u, v)
          val v2 = getVertex(u + 1, v)
          val v3 = getVertex(u + 1, v + 1)
          val v4 = getVertex(u, v + 1)

          if (((vec.y - v1.y) * (v1.x - v2.x) - (vec.x - v1.x) * (v1.y - v2.y)) >= 0 &&
            ((vec.y - v2.y) * (v2.x - v3.x) - (vec.x - v2.x) * (v2.y - v3.y)) >= 0 &&
            ((vec.y - v3.y) * (v3.x - v4.x) - (vec.x - v3.x) * (v3.y - v4.y)) >= 0 &&
            ((vec.y - v4.y) * (v4.x - v1.x) - (vec.x - v4.x) * (v4.y - v1.y)) >= 0) {
            color = Some(getColor(u, v))
            dist = v1.z
            found = true
          }
          v += 1
        }
        u += 1
      }
      color match {
        case None => None
        case Some(x) => Some(dist, x)
      }
    } else
      None
  }

}

object ProjectedMicropolygonGrid {
  val DICE_COUNT = 8
  val SPLIT_THRESHOLD = 64
  val MAX_SPLIT = 20
  val SAMPLE_RATE = 1.0f
}
