package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Matrix4, Vector3}
import math._
import edu.utexas.cs.sdao.reyes.render.Camera
import edu.utexas.cs.sdao.reyes.geom.Surface

/**
 * Creates a spotlight with cone geometry, emanating from a point
 * towards a specific direction, with a given hotspot angle and outer angle.
 *
 * @param origin the location of the spotlight, in world coordinates
 * @param direction the direction that the light points in
 * @param hotspotAngle the angle of the hotspot (inner cone), in radians,
 *                     measured from the center of the hotspot to the edge of the hotspot cone;
 *                     acceptable values are 0 < hotspotAngle < outerAngle
 * @param outerAngle the angle of the outer cone, in radians,
 *                   measured from the center of the hotspot to the edge of the outer cone;
 *                   acceptable values are hotspotAngle < outerAngle < Pi/2
 * @param shadowMapRes the length and width, in pixels, of the shadow map to generate
 * @param attenuateConst the constant attenuation factor
 * @param attenuateLin the linear attenuation factor
 * @param attenuateQuad the quadratic attenuation factor
 */
case class SpotLight(origin: Vector3, direction: Vector3,
                hotspotAngle: Float, outerAngle: Float,
                shadowMapRes: Int = 512,
                attenuateConst: Float = 0.3f,
                attenuateLin: Float = 0.6f,
                attenuateQuad: Float = 0.1f) extends Light {

  if (hotspotAngle <= 0.0 || hotspotAngle >= outerAngle)
    throw new IllegalArgumentException("hotspotAngle out of range")
  if (outerAngle >= Pi/2.0)
    throw new IllegalArgumentException("outerAngle out of range")

  val normalizedDirection = direction.normalize
  val magnitude = direction.length
  private val cosHotspotAngle = cos(hotspotAngle).toFloat
  private val cosOuterAngle = cos(outerAngle).toFloat

  private val cam = new Camera(Matrix4.lookAt(direction).translate(origin).invert,
    outerAngle * 2.0f, /* Field of view measured edge-to-edge, not center-to-edge. */
    shadowMapRes,
    shadowMapRes)

  /**
   * Calculates the light intensity at a point based on the light source.
   * @param pt the point to illuminate
   * @param normal the normal at the point to illuminate
   * @return the light intensity
   */
  def apply(pt: Vector3, normal: Vector3): Float = {
    val light = pt - origin
    val dir = light.normalize
    val spot = smoothstep(cosOuterAngle, cosHotspotAngle, dir dot normalizedDirection)
    val attenuation = (attenuateConst + attenuateLin * light.length + attenuateQuad * pow(light.length, 2.0f)).toFloat
    val shad = shadow(cam.project(pt))
    val power = magnitude * spot * shad / attenuation
    dir dot normal * power
  }

  /**
   * Renders the shadow map for the given objects.
   * Remove objects from the list of surfaces if you wish for them
   * not to cast any shadows.
   * If this function is never called before final rendering,
   * no shadows will be generated for this light.
   * @param surfaces the shadow casters
   */
  def renderShadowMap(surfaces: Iterable[Surface]) = {
    cam.render(surfaces, displaceOnly = true)
  }

  /**
   * Returns 0 if the value is below the minimum and 1 if the value is above the maximum.
   * Otherwise, it returns a smooth Hermite interpolation between the min and max.
   *
   * See [[http://http.developer.nvidia.com/CgTutorial/cg_tutorial_chapter05.html this Nvidia page]]
   * for more info.
   * @param low the minimum value
   * @param high the maximum value
   * @param x the value to smoothstep interpolate
   * @return the smoothstep-interpolated value
   */
  private def smoothstep(low: Float, high: Float, x: Float): Float = {
    if (x < low) 0.0f
    else if (x >= high) 1.0f
    else {
      (-2.0f * pow((x - low)/(high-low), 3.0f) +
        3.0f * pow((x - low)/(high-low), 2.0f)).toFloat
    }
  }

  /**
   * Given a floating-point coordinate to the z-buffer's dimensions,
   * returns a bilinearly filtered shadow multiplier value by
   * interpolating nearby points.
   *
   * A return value of 0.0 indicates that the point is to be
   * completely shadowed, whereas a value of 1.0 indicates that the
   * point is to be completely visible.
   *
   * @param pt the point at which to sample
   * @return the shadow multiplier
   */
  private def shadow(pt: Vector3): Float = {
    if (floor(pt.x) < 0 || floor(pt.y) < 0 ||
      ceil(pt.x) >= cam.zBuffer.width || ceil(pt.y) >= cam.zBuffer.height) {
      1.0f
    } else {
      val x1 = floor(pt.x).toInt
      val x2 = ceil(pt.x).toInt

      val y1 = floor(pt.y).toInt
      val y2 = ceil(pt.y).toInt

      val z = pt.z + 0.01f // Add small increment to prevent z-fighting with previous rendering.

      val xTail = pt.x - x1
      val xHead = 1.0f - xTail
      val yTail = pt.y - y1
      val yHead = 1.0f - yTail

      (if (cam.zBuffer.canPaint(x1, y1, z)) 1.0f else 0.0f) * xHead * yHead +
        (if (cam.zBuffer.canPaint(x2, y1, z)) 1.0f else 0.0f) * xTail * yHead +
        (if (cam.zBuffer.canPaint(x1, y2, z)) 1.0f else 0.0f) * xHead * yTail +
        (if (cam.zBuffer.canPaint(x2, y2, z)) 1.0f else 0.0f) * xTail * yTail
    }
  }

}
