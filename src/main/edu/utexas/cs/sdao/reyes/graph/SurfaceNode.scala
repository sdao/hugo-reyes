package edu.utexas.cs.sdao.reyes.graph

import edu.utexas.cs.sdao.reyes.core.{BoundingSphere, EmptyBoundingSphere, FilledBoundingSphere, Matrix4}
import edu.utexas.cs.sdao.reyes.geom.{TransformedSurface, Surface}
import edu.utexas.cs.sdao.reyes.anim.Animatable

/**
 * Represents a node for a surface, along with any potential child nodes in its hierarchy.
 * @param surface the surface at the node
 * @param transformMatrix the transformation matrix for the node and its hierarchy
 * @param children the child nodes of the current node
 * @param boundingSphere a sphere that bounds the current surface and its
 *                       entire hierarchy; only provide this value if the actual bounding
 *                       sphere of the hierarchy is smaller than the union of all of its
 *                       members' bounding spheres
 */
class SurfaceNode(surface: Surface,
                  transformMatrix: Animatable[Matrix4] = Matrix4.IDENTITY,
                  children: Vector[SurfaceNode] = Vector.empty,
                  val boundingSphere: Option[Animatable[BoundingSphere]] = None) {

  /**
   * The node's surface, transformed using the current transformation matrix.
   * @return the transformed surface
   */
  def transformedSurface: Surface = {
    surface.transform(transformMatrix())
  }

  /**
   * The node's child nodes, transformed using the current transformation matrix.
   * Each new matrix is the child node's transformation matrix left-multiplied by the current (parent) matrix.
   * @return the transformed child nodes
   */
  def transformedChildren: Vector[SurfaceNode] = {
    val t = transformMatrix()
    children.map(_.transform(t))
  }

  /**
   * Transforms the current node, left-multiplying its transformation matrix by another matrix.
   * @param newTransform the new transform to add (by left-multiplication)
   * @return the transformed node
   */
  def transform(newTransform: Matrix4) = {
    SurfaceNode(surface, newTransform * transformMatrix(), children, boundingSphere)
  }

}

object SurfaceNode {

  def apply(surface: Surface,
            transformMatrix: Animatable[Matrix4] = Matrix4.IDENTITY,
            children: Vector[SurfaceNode] = Vector.empty,
            bSphere: Option[Animatable[BoundingSphere]] = None) = {
    new SurfaceNode(surface, transformMatrix, children, bSphere)
  }

}
