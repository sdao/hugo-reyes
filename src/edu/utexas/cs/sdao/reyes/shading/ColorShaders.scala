package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Texture, Vector3, Color, Vector2}
import edu.utexas.cs.sdao.reyes.shading.LightHelpers._

object ColorShaders {

  type ColorShader = (Vector3, Vector3, Vector2) => Color

  def solid(c: Color): ColorShader = {
    (pt, norm, uv) => c
  }

  /**
   * Creates a diffuse shader with the specified surface color
   * and the specified light.
   * @param c the color of the surface
   * @param light the light used as an illumination source
   * @return a diffuse shader
   */
  def diffuse(c: Color, light: Light): ColorShader = {
    (vtx, normal, uv) => {
      c * light(vtx, normal)
    }
  }

  /**
   * Creates a diffuse shader with the specified surface texture
   * and the specified light.
   * @param t the surface sampled for colors
   * @param light the light used as an illumination source
   * @return a diffuse shader
   */
  def diffuse(t: Texture, light: Light): ColorShader = {
    (vtx, normal, uv) => {
      t.sampleColor(uv) * light(vtx, normal)
    }
  }

  def diffuse(c: Color, lights: Iterable[Light]): ColorShader = {
    (vtx, normal, uv) => {
      c * lights.total(vtx, normal)
    }
  }

  def diffuse(t: Texture, lights: Iterable[Light]): ColorShader = {
    (vtx, normal, uv) => {
      t.sampleColor(uv) * lights.total(vtx, normal)
    }
  }

  def checker(c: ColorShader, d: ColorShader): ColorShader = {
    (vtx, normal, uv) => {
      val uOff = (uv.x * 20.0f).toInt
      val vOff = (uv.y * 10.0f).toInt

      if (uOff % 2 == vOff % 2)
        c(vtx, normal, uv)
      else
        d(vtx, normal, uv)
    }
  }

  /**
   * A shader for testing UV coordinates.
   * U = 0 is the least saturated; U = 1 is the most saturated (sky blue color).
   * V = 0 is the darkest; V = 1 is the lightest.
   * @return the UV test shader
   */
  def uvTest: ColorShader = {
    (vtx, normal, uv) => Color(uv.y, uv.y + (1.0f - uv.y) * uv.x, uv.y + (1.0f - uv.y) * uv.x)
  }

  val DEFAULT = solid(Color.GREEN)

}
