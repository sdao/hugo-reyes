package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.Vector3

/**
 * Represents light emanating from a point source,
 * attenuated across a distance.
 *
 * Attenuation for the light source is controlled by a constant factor,
 * a linear factor, and a quadratic factor. For best results, the three
 * should add up to 1.0.
 *
 * @param origin the location of the point light, in world coordinates
 * @param attenuateConst the constant attenuation factor
 * @param attenuateLin the linear attenuation factor
 * @param attenuateQuad the quadratic attenuation factor
 */
case class PointLight(origin: Vector3,
                      attenuateConst: Float = 0.3f,
                      attenuateLin: Float = 0.6f,
                      attenuateQuad: Float = 0.1f)
  extends Light(attenuateConst, attenuateLin, attenuateQuad) {

  /**
   * Calculates the vector of the ray of light that reaches a point.
   * The length of the vector is the intensity of the light.
   * The direction of the vector is the direction from the light to the point.
   *
   * @param pt the point to illuminate
   * @return the light ray
   */
  def apply(pt: Vector3): Vector3 = {
    val light = pt - origin
    light.normalize / attenuate(light.length)
  }

}
