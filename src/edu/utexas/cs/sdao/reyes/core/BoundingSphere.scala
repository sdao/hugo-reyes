package edu.utexas.cs.sdao.reyes.core

import edu.utexas.cs.sdao.reyes.render.Projection

/**
 * The root class for empty and filled bounding spheres.
 */
abstract class BoundingSphere {

  /**
   * Expands the bounding sphere if it doesn't contain the specified point.
   * @param pt the point to add to the bounding sphere
   * @return the expanded bounding sphere
   */
  def expand(pt: Vector3): FilledBoundingSphere

  /**
   * Checks if a point is contained within the bounding sphere.
   * @param pt the point to check
   * @return whether the bounding sphere contains the specified point
   */
  def contains(pt: Vector3): Boolean

}

/**
 * The empty bounding sphere. It contains no points anywhere.
 */
case object EmptyBoundingSphere extends BoundingSphere {

  /**
   * Expands the bounding sphere if it doesn't contain the specified point.
   * @param pt the point to add to the bounding sphere
   * @return the expanded bounding sphere
   */
  override def expand(pt: Vector3): FilledBoundingSphere = {
    FilledBoundingSphere(pt, 0.0f)
  }

  /**
   * Checks if a point is contained within the bounding sphere.
   * Note that this always returns false, as no point is in the
   * empty bounding sphere.
   * @param pt the point to check
   * @return whether the bounding sphere contains the specified point
   */
  override def contains(pt: Vector3): Boolean = false

}

/**
 * A bounding sphere containing at least one point.
 * @param origin the origin of the sphere
 * @param radius the radius of the sphere
 */
case class FilledBoundingSphere(origin: Vector3, radius: Float) extends BoundingSphere {

  /**
   * Expands the bounding sphere if it doesn't contain the specified point.
   * @param pt the point to add to the bounding sphere
   * @return the expanded bounding sphere
   */
  override def expand(pt: Vector3): FilledBoundingSphere = {
    if (contains(pt)) {
      this
    } else {
      // Expand bounding sphere to include new point.
      val ptToOrigin = origin - pt
      val originToEdge = ptToOrigin.normalize * radius
      val ptToEdge = ptToOrigin + originToEdge
      val halfPtToEdge = ptToEdge * 0.5f
      val newOrigin = pt + halfPtToEdge
      val newRadius = halfPtToEdge.length
      new FilledBoundingSphere(newOrigin, newRadius)
    }
  }

  /**
   * Checks if a point is contained within the bounding sphere.
   * @param pt the point to check
   * @return whether the bounding sphere contains the specified point
   */
  override def contains(pt: Vector3): Boolean = {
    val distSquared = origin.distSquared(pt)
    distSquared <= radius * radius
  }

  /**
   * Projects the bounding box from world-space to a 2-D bounding box in screen-space.
   * The z-bounds on the resultant bounding box represent bounds
   * on the z-depth.
   * @param proj the projection to use
   * @return the projected bounding box
   */
  def project(proj: Projection): FilledBoundingBox = {
    val bBox = EmptyBoundingBox
      .expand(origin + Vector3(radius, radius, radius))
      .expand(origin - Vector3(radius, radius, radius))
    bBox.project(proj)
  }

}
