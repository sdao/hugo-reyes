package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.Vector3
import math._

/**
 * Creates a spotlight with cone geometry, emanating from a point
 * to a specific direction, with a given hotspot angle and outer angle.
 *
 * @param origin the location of the spotlight, in world coordinates
 * @param direction the direction that the light points in
 * @param hotspotAngle the angle of the hotspot (inner cone), in radians
 * @param outerAngle the angle of the outer cone, in radians
 * @param attenuateConst the constant attenuation factor
 * @param attenuateLin the linear attenuation factor
 * @param attenuateQuad the quadratic attenuation factor
 */
case class SpotLight(origin: Vector3, direction: Vector3,
                hotspotAngle: Float, outerAngle: Float,
                attenuateConst: Float = 0.3f,
                attenuateLin: Float = 0.6f,
                attenuateQuad: Float = 0.1f) extends Light {

  val normalizedDirection = direction.normalize
  val magnitude = direction.length
  private val cosHotspotAngle = cos(hotspotAngle).toFloat
  private val cosOuterAngle = cos(outerAngle).toFloat

  /**
   * Calculates the light intensity at a point based on the light source.
   * @param pt the point to illuminate
   * @param normal the normal at the point to illuminate
   * @return the light intensity
   */
  def apply(pt: Vector3, normal: Vector3): Float = {
    val light = pt - origin // V
    val dir = light.normalize
    val spot = smoothstep(cosOuterAngle, cosHotspotAngle, dir dot normalizedDirection)
    val attenuation = (attenuateConst + attenuateLin * light.length + attenuateQuad * pow(light.length, 2.0f)).toFloat
    val power = magnitude * spot / attenuation
    dir dot normal * power
  }

  /**
   * Returns 0 if the value is below the minimum and 1 if the value is above the maximum.
   * Otherwise, it returns a smooth Hermite interpolation between the min and max.
   *
   * See [[http://http.developer.nvidia.com/CgTutorial/cg_tutorial_chapter05.html this Nvidia page]]
   * for more info.
   * @param low the minimum value
   * @param high the maximum value
   * @param x the value to smoothstep interpolate
   * @return the smoothstep-interpolated value
   */
  private def smoothstep(low: Float, high: Float, x: Float): Float = {
    if (x < low) 0.0f
    else if (x >= high) 1.0f
    else {
      (-2.0f * pow((x - low)/(high-low), 3.0f) +
        3.0f * pow((x - low)/(high-low), 2.0f)).toFloat
    }
  }

}
