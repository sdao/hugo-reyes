package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Color, Vector3}
import scala.math._
import edu.utexas.cs.sdao.reyes.render.Projection

/**
 * Represents light emanating from a point source,
 * attenuated across a distance.
 *
 * Attenuation for the light source is controlled by a constant factor,
 * a linear factor, and a quadratic factor. For best results, the three
 * should add up to 1.0.
 *
 * @param origin the location of the point light, in world coordinates
 * @param color the color of the point light;
 *              this controls the magnitude of the light per color component
 * @param attenuateConst the constant attenuation factor
 * @param attenuateLin the linear attenuation factor
 * @param attenuateQuad the quadratic attenuation factor
 */
case class PointLight(origin: Vector3,
                      color: Color = Color.WHITE,
                      attenuateConst: Float = 0.3f,
                      attenuateLin: Float = 0.6f,
                      attenuateQuad: Float = 0.1f)
  extends Light(attenuateConst, attenuateLin, attenuateQuad) {

  /**
   * Calculates the light intensity at a point based on the light source.
   * @param pt the point to illuminate
   * @param normal the normal at the point to illuminate
   * @param eye the projection representing the eye position and orientation
   * @return the light intensity
   */
  def apply(pt: Vector3, normal: Vector3, eye: Projection): LightComponents = {
    val light = origin - pt
    val dir = light.normalize
    val attenuation = attenuate(light.length)
    val diffuse = dir dot normal / attenuation
    LightComponents(color * diffuse, Color.BLACK,Color.BLACK)
  }

}
