package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Vector3, Color, Vector2}
import math._

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
   * Attenuation for the light source is controlled by a constant factor,
   * a linear factor, and a quadratic factor. For best results, the three
   * should add up to 1.0.
   * @param c the color of the surface
   * @param pointLight the location of the point light, in world coordinates
   * @param magnitude the magnitude of the point light
   * @param attenuateConst the constant attenuation factor
   * @param attenuateLin the linear attenuation factor
   * @param attenuateQuad the quadratic attenuation factor
   * @return
   */
  def diffuse(c: Color, pointLight: Vector3, magnitude: Float,
              attenuateConst: Float = 0.3f,
              attenuateLin: Float = 0.6f,
              attenuateQuad: Float = 0.1f): ColorShader = {
    (vtx, normal, uv) => {
      val light = vtx - pointLight
      val dir = light.normalize
      val power = magnitude /
        (attenuateConst + attenuateLin * light.length + attenuateQuad * pow(light.length, 2.0f)).toFloat
      c * (dir dot normal * power)
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
