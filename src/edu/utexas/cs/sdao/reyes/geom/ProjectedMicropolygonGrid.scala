package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{FilledBoundingBox, EmptyBoundingBox, Camera, Vector3}
import scala.math._
import scala.Some

class ProjectedMicropolygonGrid(width: Int,
                                height: Int,
                                data: Array[(Vector3, Vector3)],
                                cam: Camera)
  extends MicropolygonGrid(width, height, data) {

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

    if (max(maxUDist, maxVDist) > ProjectedMicropolygonGrid.SPLIT_THRESHOLD) {
      if (maxUDist > maxVDist)
        Some(SplitU)
      else
        Some(SplitV)
    } else {
      None
    }
  }

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
          lowBound.y <= cam.height / 2.0f
    }
  }

}

object ProjectedMicropolygonGrid {
  val DICE_COUNT = 8
  val SPLIT_THRESHOLD = 32
}
