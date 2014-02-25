package edu.utexas.cs.sdao.reyes.graph

import edu.utexas.cs.sdao.reyes.core.{BoundingSphere, EmptyBoundingSphere, FilledBoundingSphere, Matrix4}
import edu.utexas.cs.sdao.reyes.geom.{TransformedSurface, Surface}

/**
 * Represents a node for a surface, along with any potential child nodes in its hierarchy.
 * @param surface the surface at the node
 * @param transformMatrix the transformation matrix for the node and its hierarchy
 * @param children the child nodes of the current node
 * @param bSphere a sphere that bounds the current surface and its
 *                entire hierarchy; if no bound is given, the naive bound
 *                will be calculated by combining the bounding spheres
 *                of the current surface and its hierarchy
 */
class SurfaceNode(surface: Surface,
                  transformMatrix: Matrix4 = Matrix4.IDENTITY,
                  children: Vector[SurfaceNode] = Vector.empty,
                  bSphere: Option[BoundingSphere] = None) {

  lazy val bounds =
    bSphere match {
      case Some(b) => b
      case None => transformedChildren.foldLeft(transformedSurface.boundingSphere)((accum, cur) => accum.expand(cur.boundingSphere))
    }

  /**
   * The node's surface, transformed using the current transformation matrix.
   * @return the transformed surface
   */
  def transformedSurface: Surface = {
    surface.transform(transformMatrix)
  }

  /**
   * The node's child nodes, transformed using the current transformation matrix.
   * Each new matrix is the child node's transformation matrix left-multiplied by the current (parent) matrix.
   * @return the transformed child nodes
   */
  def transformedChildren: Vector[SurfaceNode] = {
    children.map(_.transform(transformMatrix))
  }

  /**
   * Transforms the current node, left-multiplying its transformation matrix by another matrix.
   * @param newTransform the new transform to add (by left-multiplication)
   * @return the transformed node
   */
  def transform(newTransform: Matrix4) = {
    SurfaceNode(surface, newTransform * transformMatrix, children, bSphere)
  }

  /**
   * The bounding sphere of the node's surface and all descendant surfaces
   * in the hierarchy.
   * The bounding sphere can be larger than the surfaces, but must not be smaller.
   * @return a sphere containing the bounds of the surfaces
   */
  def boundingSphere: BoundingSphere = bounds

}

object SurfaceNode {

  def apply(surface: Surface,
            transformMatrix: Matrix4 = Matrix4.IDENTITY,
            children: Vector[SurfaceNode] = Vector.empty,
            bSphere: Option[BoundingSphere] = None) = {
    new SurfaceNode(surface, transformMatrix, children, bSphere)
  }

}
