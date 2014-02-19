package edu.utexas.cs.sdao.reyes.graph

import edu.utexas.cs.sdao.reyes.core.{FilledBoundingSphere, Matrix4}
import edu.utexas.cs.sdao.reyes.geom.{TransformedSurface, Surface}

/**
 * Represents a node for a surface, along with any potential child nodes in its hierarchy.
 * @param surface the surface at the node
 * @param transform the transformation matrix for the node and its hierarchy
 * @param children the child nodes of the current node
 * @param tighterBoundingSphere a sphere that bounds the current surface and its
 *                              entire hierarchy, if such a bound would be tighter than the
 *                              naive bound calculated by combining the bounding spheres
 *                              of the children
 */
class SurfaceNode(surface: Surface,
                  transform: Matrix4,
                  children: Vector[SurfaceNode],
                  tighterBoundingSphere: Option[FilledBoundingSphere]) {

  def flatten: Vector[TransformedSurface] = {
    val allNodes = children.map(_.flatten).flatten :+ surface
    allNodes.map(_.transform(transform))
  }

}
