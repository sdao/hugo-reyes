package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.geom.SplitSurface
import edu.utexas.cs.sdao.reyes.core.{EmptyBoundingBox, FilledBoundingBox, BoundingBox}

/**
 * Stores information on how to process a specific surface
 * through the pipeline after it has been split.
 * @param surface the surface to dice
 * @param boundingBox an estimate on the surface's bounding box
 * @param diceRateU the number of dices along the U-axis
 * @param diceRateV the number of dices along the V-axis
 */
case class PipelineInfo(surface: SplitSurface,
                        boundingBox: BoundingBox,
                        diceRateU: Int, diceRateV: Int) {

  def zDepth: Float = boundingBox match {
    case EmptyBoundingBox() => Float.NegativeInfinity
    case FilledBoundingBox(lowBound, upBound) => upBound.z
  }

  def dice: MicropolygonGrid = surface.dice(diceRateU, diceRateV)

}