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

  /**
   * The greatest z-depth point in the surface's displaced bounding box,
   * i.e. the z-depth of the displaced point closest to the camera.
   * @return the z-depth of the displaced surface
   */
  def zDepth: Float = boundingBox match {
    case EmptyBoundingBox => Float.NegativeInfinity
    case FilledBoundingBox(lowBound, upBound) => upBound.z
  }

  /**
   * Dice the surface using the pre-calculated U and V dice rates.
   * @return the diced micropolygon grid
   */
  def dice: MicropolygonGrid = surface.dice(diceRateU, diceRateV)

}