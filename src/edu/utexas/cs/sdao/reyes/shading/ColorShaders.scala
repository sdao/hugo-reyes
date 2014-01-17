package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Vector3, Color, Vector2}

object ColorShaders {

  type ColorShader = (Vector3, Vector2) => Color

  def solid(c: Color): ColorShader = {
    (norm, uv) => c
  }

  /**
   * Creates a diffuse shader with the specified surface color
   * and the specified light direction. The magnitude of the light
   * vector will be used for the intensity of the light.
   * @param c the color of the surface
   * @param light the direction of the light emission
   *              (i.e. negative the relative direction of the light source)
   * @return a diffuse shader
   */
  def diffuse(c: Color, light: Vector3): ColorShader = {
    (norm, uv) => {
      c * (light dot norm)
    }
  }

  def diffuse(t: Texture, light: Vector3): ColorShader = {
    (norm, uv) => {
      t(uv) * (light dot norm)
    }
  }

  def checker(c: ColorShader, d: ColorShader): ColorShader = {
    (norm, uv) => {
      val uOff = (uv.x * 20.0f).toInt
      val vOff = (uv.y * 10.0f).toInt

      if (uOff % 2 == vOff % 2)
        c(norm, uv)
      else
        d(norm, uv)
    }
  }

  /**
   * A shader for testing UV coordinates.
   * U = 0 is the least saturated; U = 1 is the most saturated (sky blue color).
   * V = 0 is the darkest; V = 1 is the lightest.
   * @return the UV test shader
   */
  def uvTest: ColorShader = {
    (norm, uv) => Color(uv.y, uv.y + (1.0f - uv.y) * uv.x, uv.y + (1.0f - uv.y) * uv.x)
  }

  val DEFAULT = solid(Color.GREEN)

}
