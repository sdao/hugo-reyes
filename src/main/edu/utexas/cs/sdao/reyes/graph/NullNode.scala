package edu.utexas.cs.sdao.reyes.graph

import edu.utexas.cs.sdao.reyes.core.{Matrix4, BoundingSphere}
import edu.utexas.cs.sdao.reyes.geom.NullSurface

/**
 * Represents a node with no surface, along with any potential child nodes in its hierarchy.
 * @param transformMatrix the transformation matrix for the node and its hierarchy
 * @param children the child nodes of the current node
 * @param bSphere a sphere that bounds the current surface and its
 *                entire hierarchy; if no bound is given, the naive bound
 *                will be calculated by combining the bounding spheres
 *                of the current surface and its hierarchy
 */
class NullNode(transformMatrix: Matrix4 = Matrix4.IDENTITY,
               children: Vector[SurfaceNode] = Vector.empty,
               bSphere: Option[BoundingSphere] = None)
  extends SurfaceNode(NullSurface, transformMatrix, children, bSphere) {}

object NullNode {

  def apply(transformMatrix: Matrix4 = Matrix4.IDENTITY,
            children: Vector[SurfaceNode] = Vector.empty,
            bSphere: Option[BoundingSphere] = None) = {
    new NullNode(transformMatrix, children, bSphere)
  }

}
