package edu.utexas.cs.sdao.reyes.graph

import edu.utexas.cs.sdao.reyes.core._
import math._
import edu.utexas.cs.sdao.reyes.shading.DisplacementShaders._
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import edu.utexas.cs.sdao.reyes.shading.ColorShaders._
import edu.utexas.cs.sdao.reyes.anim.{Expression, AnimatableVector3, AnimatableTransform, Animatable}
import edu.utexas.cs.sdao.reyes.anim.StaticParams._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingSphere
import scala.Some
import edu.utexas.cs.sdao.reyes.geom.Plane

/**
 * A node with a box composed of six plane surfaces.
 */
object BoxNode {

  class MaxBoundingSphere(width: Animatable[Float],
                          length: Animatable[Float],
                          height: Animatable[Float],
                          transform: Animatable[Matrix4])
    extends Animatable[BoundingSphere] {

    def apply(): BoundingSphere = {
      val maxDim = max(width(), max(length(), height()))
      val radius = sqrt(3.0).toFloat * maxDim * 0.5f
      transform() * FilledBoundingSphere(Vector3.ZERO, radius)
    }

  }

  def apply(width: Animatable[Float],
            height: Animatable[Float],
            length: Animatable[Float],
            transform: Animatable[Matrix4],
            displace: DisplacementShader = DisplacementShaders.DEFAULT,
            color: ColorShader = ColorShaders.DEFAULT) = {

    val topBottomPlane = Plane(width, length, displace, color)
    val frontBackPlane = Plane(width, height, displace, color)
    val leftRightPlane = Plane(height, length, displace, color)

    val topNode = SurfaceNode(topBottomPlane, AnimatableTransform(translation=AnimatableVector3(y=height)))
    val bottomNode = SurfaceNode(topBottomPlane, Matrix4.rotateZ(Pi.toFloat))

    val frontNode = SurfaceNode(frontBackPlane, AnimatableTransform(
      rotation = Vector3(Pi.toFloat / 2.0f, 0.0f, 0.0f),
      translation = AnimatableVector3(y = Expression(height, h => h / 2.0f),
                                      z = Expression(length, l => l / 2.0f))
    ))
    val backNode = SurfaceNode(frontBackPlane, AnimatableTransform(
      rotation = Vector3(-Pi.toFloat / 2.0f, 0.0f, 0.0f),
      translation = AnimatableVector3(y = Expression(height, h => h / 2.0f),
                                      z = Expression(length, l => -l / 2.0f))
    ))

    val leftNode = SurfaceNode(leftRightPlane, AnimatableTransform(
      rotation = Vector3(0.0f, 0.0f, Pi.toFloat / 2.0f),
      translation = AnimatableVector3(x = Expression(width, w => -w / 2.0f),
                                      y = Expression(height, h => h / 2.0f))
    ))
    val rightNode = SurfaceNode(leftRightPlane, AnimatableTransform(
      rotation = Vector3(0.0f, 0.0f, -Pi.toFloat / 2.0f),
      translation = AnimatableVector3(x = Expression(width, w => w / 2.0f),
                                      y = Expression(height, h => h / 2.0f))
    ))

    NullNode(transform,
      Vector(topNode, bottomNode, frontNode, backNode, leftNode, rightNode),
      Some(new MaxBoundingSphere(width, height, length, transform)))
  }

}
