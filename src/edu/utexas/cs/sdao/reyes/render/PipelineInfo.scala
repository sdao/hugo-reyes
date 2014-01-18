package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.geom.SplitSurface
import edu.utexas.cs.sdao.reyes.core.BoundingBox

/**
 * Stores information on how to best dice a specific surface.
 * @param surface the surface to dice
 * @param diceRateU the number of dices along the U-axis
 * @param diceRateV the number of dices along the V-axis
 */
case class PipelineInfo(surface: SplitSurface,
                        diceRateU: Int,
                        diceRateV: Int,
                        boundingBox: BoundingBox) {

  def dice: MicropolygonGrid = surface.dice(diceRateU, diceRateV)

}