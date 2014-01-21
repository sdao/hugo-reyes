package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Texture, Vector3, Color, Vector2}
import edu.utexas.cs.sdao.reyes.shading.LightHelpers._
import edu.utexas.cs.sdao.reyes.render.Projection

/**
 * Contains pre-defined color shaders.
 */
object ColorShaders {

  /**
   * A function that takes:
   * (1) the point of the vertex,
   * (2) the normal at the vertex,
   * (3) the UV coordinate of the vertex, and
   * (4) the projection from world coordinates to the eye,
   * returning a color.
   */
  type ColorShader = (Vector3, Vector3, Vector2, Projection) => Color

  /**
   * A solid color shader.
   * @param c the color
   * @return a shader
   */
  def solid(c: Color): ColorShader = {
    (pt, norm, uv, proj) => c
  }

  /**
   * Creates a diffuse shader with the specified surface color
   * and the specified light.
   * @param c the color of the surface
   * @param light the light used as an illumination source
   * @return a diffuse shader
   */
  def diffuse(c: Color, light: Light): ColorShader = {
    (vtx, normal, uv, proj) => {
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
    (vtx, normal, uv, proj) => {
      t.sampleColor(uv) * light(vtx, normal)
    }
  }

  /**
   * Creates a diffuse shader with the specified surface color
   * and the specified lights.
   * @param c the color of the surface
   * @param lights the lights used as illumination sources
   * @return a diffuse shader
   */
  def diffuse(c: Color, lights: Iterable[Light]): ColorShader = {
    (vtx, normal, uv, proj) => {
      c * lights.total(vtx, normal)
    }
  }

  /**
   * Creates a diffuse shader with the specified surface texture
   * and the specified lights.
   * @param t the surface sampled for colors
   * @param lights the lights used as illumination sources
   * @return a diffuse shader
   */
  def diffuse(t: Texture, lights: Iterable[Light]): ColorShader = {
    (vtx, normal, uv, proj) => {
      t.sampleColor(uv) * lights.total(vtx, normal)
    }
  }

  /**
   * Creates a shader with a checkerboard pattern that alternates
   * between two shaders.
   * @param c the first shader
   * @param d the second shader
   * @return a checkboard shader
   */
  def checker(c: ColorShader, d: ColorShader): ColorShader = {
    (vtx, normal, uv, proj) => {
      val uOff = (uv.x * 20.0f).toInt
      val vOff = (uv.y * 10.0f).toInt

      if (uOff % 2 == vOff % 2)
        c(vtx, normal, uv, proj)
      else
        d(vtx, normal, uv, proj)
    }
  }

  /**
   * A shader for testing UV coordinates.
   * U = 0 is the least saturated; U = 1 is the most saturated (sky blue color).
   * V = 0 is the darkest; V = 1 is the lightest.
   * @return a UV test shader
   */
  def uvTest: ColorShader = {
    (vtx, normal, uv, proj) => Color(uv.y, uv.y + (1.0f - uv.y) * uv.x, uv.y + (1.0f - uv.y) * uv.x)
  }

  val DEFAULT = solid(Color.GREEN)

}
