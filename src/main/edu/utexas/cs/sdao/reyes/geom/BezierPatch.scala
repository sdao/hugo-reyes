package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import edu.utexas.cs.sdao.reyes.core.{EmptyBoundingSphere, Vector3, BoundingSphere}
import math._

/**
 * Defines a Bézier patch of degree (3, 3), that is, one with
 * four control points along each parameter.
 *
 * @param control the control points of the patch, arranged in u-major order,
 *                i.e. indices 0-3 are all along the top u-edge, etc.
 *                Note that the ordering of the vertices will determine the
 *                direction of the faces and the normals; the normal direction
 *                is roughly -[(P_03 - P_00) cross (P_30 - P_00)].
 * @param displace the displacement shader
 * @param color the color shader
 */
class BezierPatch(control: Vector[Vector3],
                  displace: DisplacementShaders.DisplacementShader = DisplacementShaders.DEFAULT,
                  color: ColorShaders.ColorShader = ColorShaders.DEFAULT)
  extends Surface(displace, color) {

  if (control.length != 16)
    throw new IllegalArgumentException("Dim of control point array != 16")

  private lazy val bounds = control.foldLeft(EmptyBoundingSphere: BoundingSphere)((accum, cur) => accum.expand(cur))

  private def idx(i: Int, j: Int) = i * 4 + j

  private def b_3_0(u: Float): Float = {
    math.pow(1 - u, 3.0).toFloat
  }

  private def b_3_1(u: Float): Float = {
    (3.0 * u * math.pow(1 - u, 2.0)).toFloat
  }

  private def b_3_2(u: Float): Float = {
    (3.0 * math.pow(u, 2.0) * (1-u)).toFloat
  }

  private def b_3_3(u: Float): Float = {
    math.pow(u, 3.0).toFloat
  }

  private def deriv_b_3_0(u: Float): Float = {
    (-3.0 * math.pow(1 - u, 2.0)).toFloat
  }

  private def deriv_b_3_1(u: Float): Float = {
    (3.0 * math.pow(1 - u, 2.0) - 6.0 * (1 - u) * u).toFloat
  }

  private def deriv_b_3_2(u: Float): Float = {
    (6.0 * (1 - u) * u - 3.0 * math.pow(u, 2.0)).toFloat
  }

  private def deriv_b_3_3(u: Float): Float = {
    (3.0 * math.pow(u, 2.0)).toFloat
  }

  /**
   * Gets the world-space normal at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world-space normal
   */
  override def getNormal(u: Float, v: Float): Vector3 = evalNormal(u, 1.0f - v)

  private def evalNormal(u: Float, v: Float): Vector3 = {
    val du = control(idx(0, 0)) * (deriv_b_3_0(u) * b_3_0(v)) +
      control(idx(0, 1)) * (deriv_b_3_0(u) * b_3_1(v)) +
      control(idx(0, 2)) * (deriv_b_3_0(u) * b_3_2(v)) +
      control(idx(0, 3)) * (deriv_b_3_0(u) * b_3_3(v)) +
      control(idx(1, 0)) * (deriv_b_3_1(u) * b_3_0(v)) +
      control(idx(1, 1)) * (deriv_b_3_1(u) * b_3_1(v)) +
      control(idx(1, 2)) * (deriv_b_3_1(u) * b_3_2(v)) +
      control(idx(1, 3)) * (deriv_b_3_1(u) * b_3_3(v)) +
      control(idx(2, 0)) * (deriv_b_3_2(u) * b_3_0(v)) +
      control(idx(2, 1)) * (deriv_b_3_2(u) * b_3_1(v)) +
      control(idx(2, 2)) * (deriv_b_3_2(u) * b_3_2(v)) +
      control(idx(2, 3)) * (deriv_b_3_2(u) * b_3_3(v)) +
      control(idx(3, 0)) * (deriv_b_3_3(u) * b_3_0(v)) +
      control(idx(3, 1)) * (deriv_b_3_3(u) * b_3_1(v)) +
      control(idx(3, 2)) * (deriv_b_3_3(u) * b_3_2(v)) +
      control(idx(3, 3)) * (deriv_b_3_3(u) * b_3_3(v))

    val dv = control(idx(0, 0)) * (b_3_0(u) * deriv_b_3_0(v)) +
      control(idx(0, 1)) * (b_3_0(u) * deriv_b_3_1(v)) +
      control(idx(0, 2)) * (b_3_0(u) * deriv_b_3_2(v)) +
      control(idx(0, 3)) * (b_3_0(u) * deriv_b_3_3(v)) +
      control(idx(1, 0)) * (b_3_1(u) * deriv_b_3_0(v)) +
      control(idx(1, 1)) * (b_3_1(u) * deriv_b_3_1(v)) +
      control(idx(1, 2)) * (b_3_1(u) * deriv_b_3_2(v)) +
      control(idx(1, 3)) * (b_3_1(u) * deriv_b_3_3(v)) +
      control(idx(2, 0)) * (b_3_2(u) * deriv_b_3_0(v)) +
      control(idx(2, 1)) * (b_3_2(u) * deriv_b_3_1(v)) +
      control(idx(2, 2)) * (b_3_2(u) * deriv_b_3_2(v)) +
      control(idx(2, 3)) * (b_3_2(u) * deriv_b_3_3(v)) +
      control(idx(3, 0)) * (b_3_3(u) * deriv_b_3_0(v)) +
      control(idx(3, 1)) * (b_3_3(u) * deriv_b_3_1(v)) +
      control(idx(3, 2)) * (b_3_3(u) * deriv_b_3_2(v)) +
      control(idx(3, 3)) * (b_3_3(u) * deriv_b_3_3(v))

    (du cross dv).normalize
  }

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  override def getVertex(u: Float, v: Float): Vector3 = evalVertex(u, 1.0f - v)

  private def evalVertex(u: Float, v: Float): Vector3 = {
    control(idx(0, 0)) * (b_3_0(u) * b_3_0(v)) +
      control(idx(0, 1)) * (b_3_0(u) * b_3_1(v)) +
      control(idx(0, 2)) * (b_3_0(u) * b_3_2(v)) +
      control(idx(0, 3)) * (b_3_0(u) * b_3_3(v)) +
      control(idx(1, 0)) * (b_3_1(u) * b_3_0(v)) +
      control(idx(1, 1)) * (b_3_1(u) * b_3_1(v)) +
      control(idx(1, 2)) * (b_3_1(u) * b_3_2(v)) +
      control(idx(1, 3)) * (b_3_1(u) * b_3_3(v)) +
      control(idx(2, 0)) * (b_3_2(u) * b_3_0(v)) +
      control(idx(2, 1)) * (b_3_2(u) * b_3_1(v)) +
      control(idx(2, 2)) * (b_3_2(u) * b_3_2(v)) +
      control(idx(2, 3)) * (b_3_2(u) * b_3_3(v)) +
      control(idx(3, 0)) * (b_3_3(u) * b_3_0(v)) +
      control(idx(3, 1)) * (b_3_3(u) * b_3_1(v)) +
      control(idx(3, 2)) * (b_3_3(u) * b_3_2(v)) +
      control(idx(3, 3)) * (b_3_3(u) * b_3_3(v))
  }

  /**
   * The bounding sphere of the surface.
   * The bounding sphere can be larger than the surface, but must not be smaller.
   * @return a sphere containing the bounds of the surface
   */
  override def boundingSphere: BoundingSphere = bounds

}
