package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Color, Vector2}

object ColorShaders {

  def solid(c: Color): (Vector3, Vector2) => Color = {
    (norm, uv) => c
  }

  /**
   * Creates a diffuse shader with the specified surface color
   * and the specified light direction. The magnitude of the light
   * vector will be used for the intensity of the light.
   * @return a diffuse shader
   */
  def diffuse(c: Color, light: Vector3): (Vector3, Vector2) => Color = {
    (norm, uv) => {
      val lightNormalized = light.normalize
      val lightMagnitude = light.length
      c * (lightNormalized dot norm) * lightMagnitude
    }
  }

}
