package edu.utexas.cs.sdao.reyes.geom

import math._
import edu.utexas.cs.sdao.reyes.core.{Color, Vector2}
import edu.utexas.cs.sdao.reyes.render.{MicropolygonGrid, ProjectedMicropolygonGrid}

/**
 * Represents a split surface that is a portion of a parameterized surface.
 * @param surface the original surface
 * @param startU the starting U translation
 * @param endU the ending U translation
 * @param startV the starting V translation
 * @param endV the ending V translation
 * @param splitCount the number of times the current piece has been split
 *                   from the original
 */
case class SplitSurface(surface: Surface,
                        startU: Float = 0.0f, endU: Float = 1.0f,
                        startV: Float = 0.0f, endV: Float = 1.0f,
                        splitCount: Int = 0) {

  /**
   * Splits the surface into two new surfaces along the specified axis.
   * @param direction the axis to split on
   * @return the new split surfaces
   */
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

  /**
   * Dices the split surface into a micropolygon grid with the specified
   * number of divisions.
   * @param uDivisions the number of divisions along the U-axis
   * @param vDivisions the number of divisions along the V-axis
   * @return the diced micropolygon grid
   */
  def dice(uDivisions: Int = MicropolygonGrid.INITIAL_DICE_COUNT,
           vDivisions: Int = MicropolygonGrid.INITIAL_DICE_COUNT): MicropolygonGrid = {
    val width = endU - startU
    val height = endV - startV

    val data =
      (0 until uDivisions).flatMap(uIndex => {
        val u = startU + width * uIndex.toFloat / (uDivisions - 1).toFloat
        (0 until vDivisions).map(vIndex => {
          val v = startV + height * vIndex.toFloat / (vDivisions - 1).toFloat

          (surface.getVertex(u, v), surface.getNormal(u, v), Vector2(u, v), Color.BLACK)
        })
      }).toArray

    new MicropolygonGrid(uDivisions, vDivisions, this, data)
  }

  override def toString: String = {
    f"$startU < u < $endU, $startV < v < $endV ($surface)"
  }
}

/**
 * Specifies the axis to split on.
 */
abstract class SplitDirection

/**
 * Split the U-axis, i.e. if the original surface is defined on U=a..b,
 * pick a point c such that c = a + (1/2)(b-a) to form two new surfaces
 * defined on U=a..c and U=c..b
 */
case object SplitU extends SplitDirection

/**
 * Split the V-axis, i.e. if the original surface is defined on V=a..b,
 * pick a point c such that c = a + (1/2)(b-a) to form two new surfaces
 * defined on V=a..c and V=c..b
 */
case object SplitV extends SplitDirection