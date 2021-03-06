package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{EmptyBoundingSphere, FilledBoundingSphere, FilledBoundingBox, Vector3}
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import scala.math._
import edu.utexas.cs.sdao.reyes.anim.Animatable

case class Cylinder(radius: Animatable[Float],
                    height: Animatable[Float],
                    displace: DisplacementShaders.DisplacementShader = DisplacementShaders.DEFAULT,
                    color: ColorShaders.ColorShader = ColorShaders.DEFAULT)
  extends Surface(displace, color) {

  /**
   * The bounding sphere of the surface.
   * The bounding sphere can be larger than the surface, but must not be smaller.
   * @return a sphere containing the bounds of the surface
   */
  def boundingSphere: FilledBoundingSphere = {
    val r = radius()
    val h = height()
    EmptyBoundingSphere
      .expand(Vector3(-r, 0.0f, -r))
      .expand(Vector3(r, 0.0f, r))
      .expand(Vector3(-r, h, -r))
      .expand(Vector3(r, h, r))
  }

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  def getVertex(u: Float, v: Float): Vector3 = {
    getNormal(u, v) * radius() + Vector3(0.0f, (1.0f - v) * height(), 0.0f)
  }

  /**
   * Gets the world-space normal at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world-space normal
   */
  def getNormal(u: Float, v: Float): Vector3 = {
    if (u < 0.0 || u > 1.0) {
      throw new IllegalArgumentException("u not between 0.0 and 1.0, inclusive")
    }

    // Map u ~ [0.0, 1.0] to uAngle ~ [0, 2*PI]
    val uAngle = 2.0 * Pi * u

    Vector3(sin(uAngle).toFloat,
      0.0f,
      cos(uAngle).toFloat).normalize
  }

}
