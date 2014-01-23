package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core._
import edu.utexas.cs.sdao.reyes.render.Projection
import edu.utexas.cs.sdao.reyes.core.MathHelpers._
import scala.math._

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
   * Creates a diffuse (Lambertian) shader with the specified surface map
   * and the specified light.
   * @param m the surface map sampled for colors
   * @param lights the lights used as an illumination source
   * @return a Lambert shader
   */
  def lambert(m: ColorMap, lights: Vector[Light]): ColorShader = {
    (vtx, normal, uv, proj) => {
      m.sampleColor(uv) * lights.map(l => -l(vtx) dot normal).sum
    }
  }

  /**
   * Creates a shiny (Phong) shader with the specified surface map
   * and the specified light.
   *
   * For best results, scale down the diffuse and specular maps
   * so they add up to about 1.0; a ratio of 0.8 diffuse--0.2 specular
   * is a good starting point.
   *
   * @param diffuse the surface map sampled for diffuse colors
   * @param specular the surface map sampled for specular colors
   * @param lights the lights used as an illumination source
   * @param hardness the hardness of the specular reflection (the Phong cosine power)
   * @return a Phong shader
   */
  def phong(diffuse: ColorMap,
            specular: ColorMap,
            lights: Vector[Light],
            hardness: Float = 20.0f): ColorShader = {
    (vtx, normal, uv, proj) => {
      val totalLighting = lights.map(l => {
        val ray = l(vtx)
        val diffuse = -ray dot normal
        val specular = pow(clampUnit(ray.normalize.reflect(normal) dot -proj.eyeWorldSpace), hardness).toFloat * ray.length
        (diffuse, specular)
      }).foldLeft((0.0f, 0.0f))((accum, cur) => (accum._1 + cur._1, accum._2 + cur._2))

      diffuse.sampleColor(uv) * totalLighting._1 +
        specular.sampleColor(uv) * totalLighting._2
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
