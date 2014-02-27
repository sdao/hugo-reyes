package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core._
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}

/**
 * A finite plane centered at the origin.
 * Its normal points in the Vector3.J direction, the U-axis
 * points in the Vector3.I position, and the V-axis points
 * in the Vector3.K position.
 *
 * @param width the width of the plane along the U-axis
 * @param length the length of the plane along the V-axis
 * @param displace the displacement shader
 * @param color the color shader
 */
case class Plane(width: Float,
                 length: Float,
                 displace: DisplacementShaders.DisplacementShader = DisplacementShaders.DEFAULT,
                 color: ColorShaders.ColorShader = ColorShaders.DEFAULT)
  extends Surface(displace, color) {

  private val uAxis = Vector3.I

  private val vAxis = Vector3.K

  private val normal = Vector3.J

  private val zeroPoint = Vector3.ZERO - uAxis * width / 2.0f - vAxis * length / 2.0f

  /**
   * The bounding sphere of the surface.
   * The bounding sphere can be larger than the surface, but must not be smaller.
   * @return a sphere containing the bounds of the surface
   */
  def boundingSphere: FilledBoundingSphere =
    EmptyBoundingSphere.expand(getVertex(0.0f, 0.0f)).expand(getVertex(1.0f, 1.0f))

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  def getVertex(u: Float, v: Float): Vector3 = {
    zeroPoint + uAxis * width * u + vAxis * length * v
  }

  /**
   * Gets the world-space normal at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world-space normal
   */
  def getNormal(u: Float, v: Float): Vector3 = normal

}
