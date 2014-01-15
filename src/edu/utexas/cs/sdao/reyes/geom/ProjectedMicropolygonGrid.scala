package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core._
import scala.math._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingBox
import edu.utexas.cs.sdao.reyes.core.EmptyBoundingBox
import scala.Some
import java.awt.image.BufferedImage

class ProjectedMicropolygonGrid(width: Int,
                                height: Int,
                                surface: SplitSurface,
                                data: Array[(Vector3, Vector3, Vector2, Color)],
                                cam: Camera)
  extends MicropolygonGrid(width, height, surface, data) {

  // We have to calculate the maximum length along the U- and V-axes.
  val faceDistances =
    (0 until ProjectedMicropolygonGrid.DICE_COUNT - 1).flatMap(u => {
      (0 until ProjectedMicropolygonGrid.DICE_COUNT - 1).map(v => {
        val v1 = getVertex(u, v).toVector2
        val v2 = getVertex(u + 1, v).toVector2
        val v3 = getVertex(u, v + 1).toVector2

        (v1.dist(v2), v1.dist(v3)) // (u-dist, v-dist)
      })
    })

  val maxUDist = faceDistances.map(_._1).max * ProjectedMicropolygonGrid.DICE_COUNT
  val maxVDist = faceDistances.map(_._2).max * ProjectedMicropolygonGrid.DICE_COUNT

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
  def diceInfo = DiceInfo(surface,
    max(1, ceil(maxUDist * 2.0f).toInt),
    max(1, ceil(maxVDist * 2.0f).toInt))

  /**
   * Determines if the current micropolygon grid is visible.
   * @return the visibility of the grid
   */
  def isVisible: Boolean = {
    boundingBox match {
      case EmptyBoundingBox() => false
      case FilledBoundingBox(lowBound, upBound) =>
        upBound.x >= -cam.width / 2.0f &&
          lowBound.x <= cam.width / 2.0f &&
          upBound.y >= -cam.height / 2.0f &&
          lowBound.y <= cam.height / 2.0f &&
          -lowBound.z >= cam.near /* must negate z, since using the OpenGL convention
                                     of pointing towards the negative z-axis */
    }
  }

  def rasterize(buffer: BufferedImage, zBuffer: ZBuffer) = {
    // Bust into micropolygons.
    (0 until width - 1).flatMap(u => {
      (0 until height - 1).map(v => {
        val v1 = getVertex(u, v)
        val v2 = getVertex(u + 1, v)
        val v3 = getVertex(u + 1, v + 1)
        val v4 = getVertex(u, v + 1)
        val norm = getNormal(u, v)
        val color = getColor(u, v)
        Micropolygon(v1, v2, v3, v4, norm, color)
      })
    }).map(_.rasterize(buffer, zBuffer))
  }

}

object ProjectedMicropolygonGrid {
  val DICE_COUNT = 8
  val SPLIT_THRESHOLD = 16
  val MAX_SPLIT = 20
}
