package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{FilledBoundingSphere, Vector3, FilledBoundingBox, Matrix4}

/**
 * Represents a surface that has been transformed by a matrix.
 *
 * @param surface the original surface before transformation
 * @param transform the transformation matrix
 */
class TransformedSurface(surface: Surface, transform: Matrix4) extends Surface {

  private val inverseTranspose = transform.invert.transpose

  /**
   * Gets the world-space normal at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world-space normal
   */
  override def getNormal(u: Float, v: Float): Vector3 =
    inverseTranspose * surface.getNormal(u, v)

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  override def getVertex(u: Float, v: Float): Vector3 =
    transform * surface.getVertex(u, v)

  /**
   * The bounding box of the surface.
   * The bounding box can be larger than the surface, but must not be smaller.
   * @return a box containing the bounds of the surface
   */
  override def boundingSphere: FilledBoundingSphere =
    transform * surface.boundingSphere

  override def transform(newTransform: Matrix4): TransformedSurface = {
    new TransformedSurface(this, newTransform * transform)
  }

}
