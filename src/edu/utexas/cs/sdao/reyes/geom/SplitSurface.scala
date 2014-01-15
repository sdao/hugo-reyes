package edu.utexas.cs.sdao.reyes.geom

import math._

/**
 * Represents a split surface that is a portion of a parameterized surface.
 * @param surface the original surface
 * @param startU the starting U position
 * @param endU the ending U position
 * @param startV the starting V position
 * @param endV the ending V position
 * @param splitCount the number of times the current piece has been split
 *                   from the original
 */
case class SplitSurface(surface: Surface,
                        startU: Float = 0.0f, endU: Float = 1.0f,
                        startV: Float = 0.0f, endV: Float = 1.0f,
                        splitCount: Int = 0) {

  def split(direction: SplitDirection): Vector[SplitSurface] = {
    val newUVs =
      direction match {
        case SplitU =>
          val midU = this.startU + (this.endU - this.startU) / 2.0f
          (startU, midU, midU, endU, startV, endV, startV, endV)
        case SplitV =>
          val midV = this.startV + (this.endV - this.startV) / 2.0f
          (startU, endU, startU, endU, startV, midV, midV, endV)
      }

    Vector(
      SplitSurface(surface,
        newUVs._1, newUVs._2,
        newUVs._5, newUVs._6,
        splitCount + 1),
      SplitSurface(surface,
        newUVs._3, newUVs._4,
        newUVs._7, newUVs._8,
        splitCount + 1)
    )
  }

  def dice(uDivisions: Int, vDivisions: Int): MicropolygonGrid = {
    val grid = new MicropolygonGrid(uDivisions, vDivisions)

    val width = endU - startU
    val height = endV - startV
    for (uIndex <- 0 until uDivisions) {
      val u = startU + width * uIndex.toFloat / (uDivisions - 1).toFloat
      for (vIndex <- 0 until vDivisions) {
        val v = startV + height * vIndex.toFloat / (vDivisions - 1).toFloat

        grid.setVertex(uIndex, vIndex, surface.getVertex(u, v))
        grid.setNormal(uIndex, vIndex, surface.getNormal(u, v))
      }
    }

    grid
  }

}

abstract class SplitDirection
case object SplitU extends SplitDirection
case object SplitV extends SplitDirection