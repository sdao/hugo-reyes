package edu.utexas.cs.sdao.reyes.core

/**
 * Maps UV coordinates to colors.
 */
trait ColorMap {

  /**
   * Samples a color from the map.
   * @param uv the UV coordinate to sample
   * @return the color at the UV coordinate
   */
  def sampleColor(uv: Vector2): Color

}
