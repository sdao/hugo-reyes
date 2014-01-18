package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Vector3, Color, Vector2}
import edu.utexas.cs.sdao.reyes.shading.PointLightHelpers._

object ColorShaders {

  type ColorShader = (Vector3, Vector3, Vector2) => Color

  def solid(c: Color): ColorShader = {
    (pt, norm, uv) => c
  }

  /**
   * Creates a diffuse shader with the specified surface color
   * and the specified light direction. The magnitude of the light
   * vector will be used for the intensity of the light.
   * @param c the color of the surface
   * @param light the direction of the light emission from the light source
   *              (i.e. negative the relative direction of the light source)
   * @return a diffuse shader
   */
  def diffuse(c: Color, light: Vector3): ColorShader = {
    (vtx, normal, uv) => {
      c * (light dot normal)
    }
  }

  def diffuse(t: Texture, light: Vector3): ColorShader = {
    (vtx, normal, uv) => {
      t(uv) * (light dot normal)
    }
  }

  /**
   * Creates a diffuse shader with the specified color
   * and the specified point light source.
   * @param c the color of the surface
   * @param pointLight the location of the point light, in world coordinates
   * @param magnitude the magnitude of the point light
   * @return
   */
  def diffuse(c: Color, pointLight: Vector3, magnitude: Float): ColorShader = {
    val light = PointLight(pointLight, magnitude)
    (vtx, normal, uv) => {
      light.calculateDiffuse(vtx, normal, c)
    }
  }

  def diffuse(t: Texture, pointLight: Vector3, magnitude: Float): ColorShader = {
    val light = PointLight(pointLight, magnitude)
    (vtx, normal, uv) => {
      light.calculateDiffuse(vtx, normal, t(uv))
    }
  }

  def diffuse(c: Color, lights: Iterable[PointLight]): ColorShader = {
    (vtx, normal, uv) => {
      lights.calculateDiffuse(vtx, normal, c)
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
