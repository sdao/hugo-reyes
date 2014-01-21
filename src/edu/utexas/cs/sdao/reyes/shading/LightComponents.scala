package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.Color

/**
 * The intensity of a light at a vertex, with diffuse, specular, and ambient components.
 * @param diffuse the diffuse component
 * @param specular the specular component
 * @param ambient the ambient component
 */
case class LightComponents(diffuse: Color,
                           specular: Color,
                           ambient: Color) {

  def +(r: LightComponents): LightComponents =
    LightComponents(diffuse + r.diffuse, specular + r.specular, ambient + r.ambient)

}

object LightComponents {
  val ZERO = LightComponents(Color.BLACK, Color.BLACK, Color.BLACK)
}
