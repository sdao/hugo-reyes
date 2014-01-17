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
   * @param light the direction of the light source, relative to the surface
   *              (i.e. the negation of the direction of the light from the light source itself)
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

  val DEFAULT = solid(Color.GREEN)

}
