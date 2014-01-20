package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{FilledBoundingBox, Vector3}
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import scala.math._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingBox

case class Cylinder(radius: Float,
                    height: Float,
                    origin: Vector3,
                    displace: DisplacementShaders.DisplacementShader = DisplacementShaders.DEFAULT,
                    color: ColorShaders.ColorShader = ColorShaders.DEFAULT)
  extends Surface(displace, color) {

  private val top = origin + Vector3(0.0f, height, 0.0f)

  def boundingBox: FilledBoundingBox =
    FilledBoundingBox(origin - Vector3(radius, 0.0f, radius),
      origin + Vector3(radius, height, radius))

  def getVertex(u: Float, v: Float): Vector3 = {
    getNormal(u, v) * radius + origin + Vector3(0.0f, (1.0f - v) * height, 0.0f)
  }

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
