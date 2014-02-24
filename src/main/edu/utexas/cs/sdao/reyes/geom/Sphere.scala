package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{FilledBoundingSphere, FilledBoundingBox, Vector3, BoundingBox}
import math._
import edu.utexas.cs.sdao.reyes.shading.{DisplacementShaders, ColorShaders}

case class Sphere(radius: Float,
                  origin: Vector3,
                  displace: DisplacementShaders.DisplacementShader = DisplacementShaders.DEFAULT,
                  color: ColorShaders.ColorShader = ColorShaders.DEFAULT)
  extends Surface(displace, color) {

  /**
   * The bounding sphere of the surface.
   * The bounding sphere can be larger than the surface, but must not be smaller.
   * @return a sphere containing the bounds of the surface
   */
  def boundingSphere: FilledBoundingSphere =
    FilledBoundingSphere(origin, radius)

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  def getVertex(u: Float, v: Float): Vector3 = {
    // Spheres have a special property:
    // The vertex at a point on the unit sphere is the same as the normal.
    getNormal(u, v) * radius + origin
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

    // Map v ~ [0.0, 1.0] to vAngle ~ [-PI/2, PI/2]
    val vAngle = -Pi * (v - 0.5)

    Vector3((cos(vAngle) * sin(uAngle)).toFloat,
      sin(vAngle).toFloat,
      (cos(vAngle) * cos(uAngle)).toFloat).normalize
  }

}
