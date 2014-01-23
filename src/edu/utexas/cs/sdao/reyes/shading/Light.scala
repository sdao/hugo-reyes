package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Color, Vector3}
import scala.math._
import edu.utexas.cs.sdao.reyes.render.Projection

/**
 * The base class for a light that provides illumination to shaders.
 * @param attenuateConst the constant attenuation factor
 * @param attenuateLin the linear attenuation factor
 * @param attenuateQuad the quadratic attenuation factor
 */
abstract class Light(attenuateConst: Float = 0.3f,
                     attenuateLin: Float = 0.6f,
                     attenuateQuad: Float = 0.1f) {

  /**
   * Calculates the vector of the ray of light that reaches a point.
   * The length of the vector is the intensity of the light.
   * The direction of the vector is the direction from the light to the point.
   *
   * @param pt the point to illuminate
   * @return the light ray
   */
  def apply(pt: Vector3): Vector3

  /**
   * Helper function to calculate attenuation.
   * @param distance the distance to attenuate for
   * @return the attenuation multiplier
   */
  protected def attenuate(distance: Float): Float =
    attenuateConst + attenuateLin * distance + attenuateQuad * distance * distance

}
