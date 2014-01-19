package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Color, Vector3}
import scala.math._

/**
 * Represents light emanating from a point source,
 * attenuated across a distance.
 *
 * Attenuation for the light source is controlled by a constant factor,
 * a linear factor, and a quadratic factor. For best results, the three
 * should add up to 1.0.
 *
 * @param origin the location of the point light, in world coordinates
 * @param magnitude the magnitude of the point light
 * @param attenuateConst the constant attenuation factor
 * @param attenuateLin the linear attenuation factor
 * @param attenuateQuad the quadratic attenuation factor
 */
case class PointLight(origin: Vector3, magnitude: Float,
                      attenuateConst: Float = 0.3f,
                      attenuateLin: Float = 0.6f,
                      attenuateQuad: Float = 0.1f) extends Light {

  /**
   * Calculates the light intensity at a point based on the light source.
   * @param pt the point to illuminate
   * @param normal the normal at the point to illuminate
   * @return the light intensity
   */
  def apply(pt: Vector3, normal: Vector3): Float = {
    val light = pt - origin
    val dir = light.normalize
    val attenuation = (attenuateConst + attenuateLin * light.length + attenuateQuad * pow(light.length, 2.0f)).toFloat
    val power = magnitude / attenuation
    dir dot normal * power
  }

}
