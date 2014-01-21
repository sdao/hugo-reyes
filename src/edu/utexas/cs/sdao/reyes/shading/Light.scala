package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.Vector3

/**
 * The base trait for a light that provides illumination to shaders.
 */
trait Light {

  /**
   * Calculates the light intensity at a point based on the light source.
   * @param pt the point to illuminate
   * @param normal the normal at the point to illuminate
   * @return the light intensity
   */
  def apply(pt: Vector3, normal: Vector3): Float

}

object LightHelpers {

  implicit class LightIterable(x: Iterable[Light]) {

    /**
     * Sums the illumination coming from several lights.
     * @param pt the point to illuminate
     * @param normal the normal at the point to illuminate
     * @return the total light intensity
     */
    def total(pt: Vector3, normal: Vector3): Float = {
      x.map(light => light(pt, normal)).foldLeft(0.0f)(_+_)
    }

  }

}
