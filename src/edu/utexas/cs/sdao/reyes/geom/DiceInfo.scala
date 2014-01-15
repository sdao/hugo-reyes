package edu.utexas.cs.sdao.reyes.geom

/**
 * Stores information on how to best dice a specific surface.
 * @param surface the surface to dice
 * @param diceRateU the number of dices along the U-axis
 * @param diceRateV the number of dices along the V-axis
 */
case class DiceInfo(surface: SplitSurface, diceRateU: Int, diceRateV: Int) {

  def dice: MicropolygonGrid = surface.dice(diceRateU, diceRateV)

}