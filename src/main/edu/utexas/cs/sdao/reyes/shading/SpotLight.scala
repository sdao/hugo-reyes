package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{ZBuffer, Matrix4, Vector3}
import math._
import edu.utexas.cs.sdao.reyes.core.MathHelpers._
import edu.utexas.cs.sdao.reyes.render.SupersamplingCamera
import edu.utexas.cs.sdao.reyes.geom.Surface
import edu.utexas.cs.sdao.reyes.graph.SurfaceNode

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
                     supersampleShadowMap: Int = 2,
                     attenuateConst: Float = 0.3f,
                     attenuateLin: Float = 0.6f,
                     attenuateQuad: Float = 0.1f)
  extends Light(attenuateConst, attenuateLin, attenuateQuad) {

  if (hotspotAngle <= 0.0 || hotspotAngle >= outerAngle)
    throw new IllegalArgumentException("hotspotAngle out of range")
  if (outerAngle >= Pi/2.0)
    throw new IllegalArgumentException("outerAngle out of range")

  val normalizedDirection = direction.normalize
  private val cosHotspotAngle = cos(hotspotAngle).toFloat
  private val cosOuterAngle = cos(outerAngle).toFloat

  /**
   * Warning: don't use this camera for rendering; only use it for projecting.
   * No image memory will be consumed since the buffers are lazy-loaded.
   */
  private val shadowMapProjection =
    new SupersamplingCamera(Matrix4.lookAt(direction).translate(origin),
      outerAngle * 2.0f, /* Field of view measured edge-to-edge, not center-to-edge. */
      shadowMapRes,
      shadowMapRes,
      supersampleShadowMap)
  private val shadowMap = new ZBuffer(shadowMapRes * supersampleShadowMap,
    shadowMapRes * supersampleShadowMap)

  /**
   * Calculates the vector of the ray of light that reaches a point.
   * The length of the vector is the intensity of the light.
   * The direction of the vector is the direction from the light to the point.
   *
   * @param pt the point to illuminate
   * @return the light ray
   */
  def apply(pt: Vector3): Vector3 = {
    val light = pt - origin
    val dir = light.normalize
    val spot = smoothstep(cosOuterAngle, cosHotspotAngle, dir dot normalizedDirection)
    val shad = shadow(shadowMapProjection.projectToScreen(pt))
    val attenuation = attenuate(light.length)
    dir * spot * shad / attenuation
  }

  /**
   * Renders the shadow map for the given objects.
   * Remove objects from the list of surfaces if you wish for them
   * not to cast any shadows.
   * If this function is never called before final rendering,
   * no shadows will be generated for this light.
   * @param root the root node of the hierarchy to render shadows for
   */
  def renderShadowMap(root: SurfaceNode) = {
    val cam = shadowMapProjection.toCamera // Camera memory should be released out of scope.
    cam.render(root, displaceOnly = true)
    cam.copyZBuffer(shadowMap)
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
      ceil(pt.x) >= shadowMap.width || ceil(pt.y) >= shadowMap.height) {
      1.0f
    } else {
      val x1 = floor(pt.x).toInt
      val x2 = ceil(pt.x).toInt

      val y1 = floor(pt.y).toInt
      val y2 = ceil(pt.y).toInt

      val z = pt.z + 0.1f // Add small increment to prevent z-fighting with previous rendering.

      val xTail = pt.x - x1
      val xHead = 1.0f - xTail
      val yTail = pt.y - y1
      val yHead = 1.0f - yTail

      (if (shadowMap.canPaint(x1, y1, z)) 1.0f else 0.0f) * xHead * yHead +
        (if (shadowMap.canPaint(x2, y1, z)) 1.0f else 0.0f) * xTail * yHead +
        (if (shadowMap.canPaint(x1, y2, z)) 1.0f else 0.0f) * xHead * yTail +
        (if (shadowMap.canPaint(x2, y2, z)) 1.0f else 0.0f) * xTail * yTail
    }
  }

}
