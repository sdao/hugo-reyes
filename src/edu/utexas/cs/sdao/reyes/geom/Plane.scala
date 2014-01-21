package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{EmptyBoundingBox, Vector3, FilledBoundingBox}
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}

/**
 * A finite plane.
 * Note that, when projected into screen space, the plane is only visible if
 * the positive V-axis is clockwise from the positive U-axis, e.g. when the U-axis
 * extends to the right and the V-axis extends to the bottom.
 *
 * @param width the width of the plane along the U-axis
 * @param length the length of the plane along the V-axis
 * @param origin the center of the plane
 * @param uAxis the axis along which the U-coordinates increase
 * @param vAxis the axis along which the V-coordinates increase
 * @param displace the displacement shader
 * @param color the color shader
 */
case class Plane(width: Float,
                 length: Float,
                 origin: Vector3,
                 uAxis: Vector3,
                 vAxis: Vector3,
                 displace: DisplacementShaders.DisplacementShader = DisplacementShaders.DEFAULT,
                 color: ColorShaders.ColorShader = ColorShaders.DEFAULT)
  extends Surface(displace, color) {

  private val uNormalized = uAxis.normalize

  private val vNormalized = vAxis.normalize

  private val normal = (uNormalized cross vNormalized).normalize

  private val zeroPoint = origin - uAxis * width / 2.0f - vAxis * length / 2.0f

  /**
   * The bounding box of the surface.
   * The bounding box can be larger than the surface, but must not be smaller.
   * @return a box containing the bounds of the surface
   */
  def boundingBox: FilledBoundingBox =
    EmptyBoundingBox.expand(getVertex(0.0f, 0.0f)).expand(getVertex(1.0f, 1.0f))

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
