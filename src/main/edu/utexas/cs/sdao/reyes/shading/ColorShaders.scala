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
   * @param hardness the hardness of the specular reflection (the Phong cosine power)
   * @param lights the lights used as an illumination source
   * @return a Phong shader
   */
  def phong(diffuse: ColorMap,
            specular: ColorMap,
            hardness: Float = 20.0f,
            lights: Vector[Light]): ColorShader = {
    (vtx, normal, uv, proj) => {
      val totalLighting = lights.map(l => {
        val ray = l(vtx)
        val diffuse = -ray dot normal
        val specular = pow(clampUnit(ray.normalize.reflect(normal) dot -proj.transformToCameraNorm(vtx)), hardness).toFloat * ray.length
        (diffuse, specular)
      }).foldLeft((0.0f, 0.0f))((accum, cur) => (accum._1 + cur._1, accum._2 + cur._2))

      diffuse.sampleColor(uv) * totalLighting._1 +
        specular.sampleColor(uv) * totalLighting._2
    }
  }

  /**
   * Creates a toon/cel shader, using Lambertian reflectance only,
   * with the specified surface map and the specified light.
   *
   * Note: this shader guesses edges by comparing the surface normal
   * with the vector from the camera; this causes surfaces nearly perpendicular
   * to the camera to appear completely black (as edges).
   *
   * @param m the surface map sampled for non-edge colors
   * @param e the surface map sampled for edge colors
   * @param levels the number of levels of shading
   * @param contour a factor that is proportional to the edge contour thickness;
   *                0.0 = no contour, 1.0 = thickest contour
   * @param smoothness a factor that controls how smooth the smoothness between the surface and contours;
   *                   0.0 = no smoothing, 1.0 = smoothed across entire surface
   * @param lights the lights used as an illumination source
   * @return a toon shader
   */
  def toonLambert(m: ColorMap, e: ColorMap, levels: Int = 3,
                  contour: Float = 0.5f, smoothness: Float = 0.1f,
                  lights: Vector[Light]): ColorShader = {
    val edgeInner = clamp(0.0f, 1.0f, contour - smoothness / 2.0f)
    val edgeOuter = clamp(0.0f, 1.0f, contour + smoothness / 2.0f)

    (vtx, normal, uv, proj) => {
      val surfaceComp = smoothstep(edgeInner, edgeOuter, normal dot -proj.transformToCameraNorm(vtx))

      val surface = m.sampleColor(uv) * (lights.map(l => -l(vtx) dot normal).sum * levels).ceil / levels
      val edge = e.sampleColor(uv)

      surface * surfaceComp + edge * (1.0f - surfaceComp)
    }
  }

  /**
   * Creates a toon/cel shader, using Lambertian reflectance only,
   * with the specified surface map and the specified light.
   *
   * Note: this shader guesses edges by comparing the surface normal
   * with the vector from the camera; this causes surfaces nearly perpendicular
   * to the camera to appear completely black (as edges).
   *
   * @param diffuse the surface map sampled for non-edge colors
   * @param specular the surface map sampled for specular colors
   * @param e the surface map sampled for edge colors
   * @param hardness the size of the specular reflection (the Phong cosine power)
   * @param levels the number of levels of shading
   * @param contour a factor that is proportional to the edge contour thickness;
   *                0.0 = no contour, 1.0 = thickest contour
   * @param smoothness a factor that controls how smooth the smoothness between the surface and contours or highlights;
   *                   0.0 = no smoothing, 1.0 = smoothed across entire surface
   * @param lights the lights used as an illumination source
   * @return a toon shader
   */
  def toonPhong(diffuse: ColorMap, specular: ColorMap, e: ColorMap,
                hardness: Float = 20.0f, levels: Int = 3,
                contour: Float = 0.5f, smoothness: Float = 0.1f,
                lights: Vector[Light]): ColorShader = {

    val edgeInner = clampUnit(contour - smoothness / 2.0f)
    val edgeOuter = clampUnit(contour + smoothness / 2.0f)

    val specularInner = clampUnit(0.5f - smoothness / 2.0f)
    val specularOuter = clampUnit(0.5f + smoothness / 2.0f)

    (vtx, normal, uv, proj) => {
      val pointToEye = -proj.transformToCameraNorm(vtx)

      val totalLighting = lights.map(l => {
        val ray = l(vtx)
        val diffuse = -ray dot normal
        val specular = pow(clampUnit(ray.normalize.reflect(normal) dot pointToEye), hardness).toFloat * ray.length
        (diffuse, specular)
      }).foldLeft((0.0f, 0.0f))((accum, cur) => (accum._1 + cur._1, accum._2 + cur._2))

      val surfaceComp = smoothstep(edgeInner, edgeOuter, normal dot pointToEye)
      val diffuseLevel = (totalLighting._1 * levels).ceil / levels
      val specularLevel = smoothstep(specularInner, specularOuter, totalLighting._2)

      val surface = diffuse.sampleColor(uv) * diffuseLevel +
        specular.sampleColor(uv) * specularLevel
      val edge = e.sampleColor(uv)

      surface * surfaceComp + edge * (1.0f - surfaceComp)
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
