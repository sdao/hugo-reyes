package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{Vector3, Color}
import scala.math._
import scala.Some

class MicropolygonGrid(width: Int, height: Int) {

  val vertices = Array.ofDim[Vector3](width, height)
  val normals = Array.ofDim[Vector3](width, height)

  def setVertex(u: Int, v: Int, vertex: Vector3) = {
    vertices(u)(v) = vertex
  }

  def getVertex(u: Int, v: Int) = vertices(u)(v)

  def setNormal(u: Int, v: Int, normal: Vector3) = {
    vertices(u)(v) = normal
  }

  def getNormal(u: Int, v: Int) = normals(u)(v)

  /**
   * Determines if the current micropolygon grid can be split and, if
   * so, on which axis.
   * @return None if the grid isn't splittable;
   *         Some[SplitDirection] if the grid should be split on the
   *         given direction
   */
  def isSplittable: Option[SplitDirection] = {
    val faceDistances =
      (0 until MicropolygonGrid.DICE_COUNT - 1).flatMap(u => {
        (0 until MicropolygonGrid.DICE_COUNT - 1).map(v => {
          val v1 = getVertex(u, v).toVector2
          val v2 = getVertex(u + 1, v).toVector2
          val v3 = getVertex(u, v + 1).toVector2

          (v1.dist(v2), v1.dist(v3)) // (u-dist, v-dist)
        })
      })

    val maxUDist = faceDistances.map(_._1).max * MicropolygonGrid.DICE_COUNT
    val maxVDist = faceDistances.map(_._2).max * MicropolygonGrid.DICE_COUNT

    if (max(maxUDist, maxVDist) > MicropolygonGrid.SPLIT_THRESHOLD) {
      if (maxUDist > maxVDist)
        Some(SplitU)
      else
        Some(SplitV)
    } else {
      None
    }
  }

}

object MicropolygonGrid {
  val DICE_COUNT = 8
  val SPLIT_THRESHOLD = 32
}
