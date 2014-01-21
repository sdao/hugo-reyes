package edu.utexas.cs.sdao.reyes.core

import math._
import edu.utexas.cs.sdao.reyes.render.Projection

/**
 * The root class for empty and filled bounding boxes.
 */
abstract class BoundingBox {

  /**
   * Expands the bounding box if it doesn't contain the specified point.
   * @param pt the point to add to the bounding box
   * @return the expanded bounding box
   */
  def expand(pt: Vector3): FilledBoundingBox

}

/**
 * The empty bounding box. It contains no points, not even
 * the zero vector (0, 0, 0).
 */
case object EmptyBoundingBox extends BoundingBox {

  /**
   * Expands the bounding box if it doesn't contain the specified point.
   * @param pt the point to add to the bounding box
   * @return the expanded bounding box
   */
  def expand(pt: Vector3): FilledBoundingBox = {
    FilledBoundingBox(pt, pt)
  }

}

/**
 * A bounding box containing at least one point.
 * @param lowBound the lower bound, i.e. the vector containing the least x, y, and z coordinates
 * @param upBound the upper bound, i.e. the vector containing the greatest x, y, and z coordinates
 */
case class FilledBoundingBox(lowBound: Vector3 = Vector3.ZERO,
                             upBound: Vector3 = Vector3.ZERO) extends BoundingBox {

  /**
   * Expands the bounding box if it doesn't contain the specified point.
   * @param pt the point to add to the bounding box
   * @return the expanded bounding box
   */
  def expand(pt: Vector3): FilledBoundingBox =
    FilledBoundingBox(
      Vector3(min(lowBound.x, pt.x), min(lowBound.y, pt.y), min(lowBound.z, pt.z)),
      Vector3(max(upBound.x, pt.x), max(upBound.y, pt.y), max(upBound.z, pt.z))
    )

  /**
   * Projects the bounding box from world-space to screen-space.
   * The z-bounds on the resultant bounding box represent bounds
   * on the z-depth.
   * @param proj the projection to use
   * @return the projected bounding box
   */
  def project(proj: Projection): FilledBoundingBox = {
    val newLow = proj.projectToScreen(lowBound)
    val newUp = proj.projectToScreen(upBound)
    EmptyBoundingBox
      .expand(newLow)
      .expand(newUp)
  }

}
