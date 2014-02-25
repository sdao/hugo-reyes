package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{EmptyBoundingSphere, Vector3, BoundingSphere}

/**
 * A surface that will never be drawn and takes up zero space.
 */
case object NullSurface extends Surface {

  /**
   * Gets the world-space normal at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world-space normal
   */
  override def getNormal(u: Float, v: Float): Vector3 = Vector3.ZERO

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  override def getVertex(u: Float, v: Float): Vector3 = Vector3.ZERO

  /**
   * The bounding sphere of the surface.
   * The bounding sphere can be larger than the surface, but must not be smaller.
   * @return a sphere containing the bounds of the surface
   */
  override def boundingSphere: BoundingSphere = EmptyBoundingSphere

}
