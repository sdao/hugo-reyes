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
                      attenuateQuad: Float = 0.1f) {

  /**
   * Calculates the diffuse shading at a point based on the light source.
   * @param pt the point to illuminate
   * @param normal the normal at the point to illuminate
   * @param color the diffuse color of the surface at the point to illuminate
   * @return the illuminated color of the point
   */
  def calculateDiffuse(pt: Vector3, normal: Vector3, color: Color) = {
    val light = pt - origin
    val dir = light.normalize
    val power = magnitude /
      (attenuateConst + attenuateLin * light.length + attenuateQuad * pow(light.length, 2.0f)).toFloat
    color * (dir dot normal * power)
  }

}

object PointLightHelpers {

  implicit class PointLightIterable(x: Iterable[PointLight]) {

    def calculateDiffuse(pt: Vector3, normal: Vector3, color: Color) = {
      val lighting = x.map(pointLight => {
        val light = pt - pointLight.origin
        val dir = light.normalize
        val power = pointLight.magnitude /
          (pointLight.attenuateConst +
            pointLight.attenuateLin * light.length +
            pointLight.attenuateQuad * pow(light.length, 2.0f)).toFloat
        dir dot normal * power
      }).foldLeft(0.0f)(_+_)
      color * lighting
    }

  }

}
