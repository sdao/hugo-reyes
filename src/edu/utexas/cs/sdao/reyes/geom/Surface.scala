package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{Vector3, BoundingBox}

/**
 * A surface parameterized over UV coordinates.
 */
trait Surface {

  /**
   * Calculates the bounding box.
   * @return A box containing the bounds of the surface.
   */
  def boundingBox: BoundingBox

  /**
   * Creates a new split surface with this surface, unsplit,
   * with the split count equal to 0.
   * @return A SplitSurface containing this surface.
   */
  def toSplitSurface: SplitSurface = SplitSurface(this)

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  def getVertex(u: Float, v: Float): Vector3

  /**
   * Gets the world-space normal at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world-space normal
   */
  def getNormal(u: Float, v: Float): Vector3

}
