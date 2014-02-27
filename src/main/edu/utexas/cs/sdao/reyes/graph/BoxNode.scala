package edu.utexas.cs.sdao.reyes.graph

import edu.utexas.cs.sdao.reyes.core.{Vector3, FilledBoundingSphere, Matrix4}
import math._
import edu.utexas.cs.sdao.reyes.shading.DisplacementShaders._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingSphere
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import edu.utexas.cs.sdao.reyes.shading.ColorShaders._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingSphere
import edu.utexas.cs.sdao.reyes.geom.Plane

/**
 * A node with a box composed of six plane surfaces.
 */
object BoxNode {

  def apply(width: Float,
            height: Float,
            length: Float,
            transform: Matrix4,
            displace: DisplacementShader = DisplacementShaders.DEFAULT,
            color: ColorShader = ColorShaders.DEFAULT) = {
    val maxDim = max(width, max(length, height))
    val radius = sqrt(3.0).toFloat * maxDim * 0.5f
    val bSphere = FilledBoundingSphere(Vector3.ZERO, radius)

    val topBottomPlane = Plane(width, length, displace, color)
    val frontBackPlane = Plane(width, height, displace, color)
    val leftRightPlane = Plane(height, length, displace, color)

    val topNode = SurfaceNode(topBottomPlane, Matrix4.translation(Vector3(0.0f, height, 0.0f)))
    val bottomNode = SurfaceNode(topBottomPlane, Matrix4.rotationZ(Pi.toFloat))

    val frontNode = SurfaceNode(frontBackPlane,
      Matrix4.rotationX(Pi.toFloat / 2.0f).translate(Vector3(0.0f, height / 2.0f, length / 2.0f)))
    val backNode = SurfaceNode(frontBackPlane,
      Matrix4.rotationX(-Pi.toFloat / 2.0f).translate(Vector3(0.0f, height / 2.0f, -length / 2.0f)))

    val leftNode = SurfaceNode(leftRightPlane,
      Matrix4.rotationZ(Pi.toFloat / 2.0f).translate(Vector3(-width / 2.0f, height / 2.0f, 0.0f)))
    val rightNode = SurfaceNode(leftRightPlane,
      Matrix4.rotationZ(-Pi.toFloat / 2.0f).translate(Vector3(width / 2.0f, height / 2.0f, 0.0f)))

    NullNode(transform,
      Vector(topNode, bottomNode, frontNode, backNode, leftNode, rightNode),
      Some(transform * bSphere))
  }

}
