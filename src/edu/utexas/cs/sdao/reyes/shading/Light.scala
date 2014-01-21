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
   * Calculates the light intensity at a point based on the light source.
   * @param pt the point to illuminate
   * @param normal the normal at the point to illuminate
   * @param eye the projection representing the eye position and orientation
   * @return the light intensity
   */
  def apply(pt: Vector3, normal: Vector3, eye: Projection): LightComponents

  /**
   * Helper function to calculate attenuation.
   * @param distance the distance to attenuate for
   * @return the attenuation multiplier
   */
  protected def attenuate(distance: Float): Float =
    (attenuateConst + attenuateLin * distance + attenuateQuad * pow(distance, 2.0f)).toFloat

}

object LightHelpers {

  implicit class LightIterable(x: Iterable[Light]) {

    /**
     * Sums the illumination coming from several lights.
     * @param pt the point to illuminate
     * @param normal the normal at the point to illuminate
     * @param eye the projection representing the eye position and orientation
     * @return the total light intensity
     */
    def total(pt: Vector3, normal: Vector3, eye: Projection): LightComponents = {
      x.map(light => light(pt, normal, eye)).foldLeft(LightComponents.ZERO)(_+_)
    }

  }

}
