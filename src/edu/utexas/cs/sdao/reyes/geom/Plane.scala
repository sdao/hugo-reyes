package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{BoundingBox, Vector3, FilledBoundingBox}
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}

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

  def boundingBox: FilledBoundingBox =
    BoundingBox.empty.expand(getVertex(0.0f, 0.0f)).expand(getVertex(1.0f, 1.0f))

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
