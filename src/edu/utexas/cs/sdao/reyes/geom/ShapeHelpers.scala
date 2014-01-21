package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.Vector3
import edu.utexas.cs.sdao.reyes.shading.DisplacementShaders.DisplacementShader
import edu.utexas.cs.sdao.reyes.shading.ColorShaders.ColorShader

/**
 * Helpers for constructing shapes out of multiple surfaces.
 */
object ShapeHelpers {

  /**
   * Creates a box composed of six planes,
   * with the width along the world X-axis, the length along the Z-axis,
   * and the height along the Y-axis.
   * @param width the width of the box
   * @param length the length of the box
   * @param height the height of the box
   * @param origin the center of the bottom face
   * @param displace the displacement shader
   * @param color the color shader
   * @return a vector of six planes composing the box
   */
  def box(width: Float, length: Float, height: Float,
          origin: Vector3,
          displace: DisplacementShader,
          color: ColorShader): Vector[Surface] = {
    val front = Plane(width, height,
      origin + Vector3(0.0f, height / 2.0f, length / 2.0f),
      Vector3.I, -Vector3.J,
      displace, color)

    val left = Plane(length, height,
      origin + Vector3(-width / 2.0f, height / 2.0f, 0.0f),
      Vector3.K, -Vector3.J,
      displace, color)

    val back = Plane(width, height,
      origin + Vector3(0.0f, height / 2.0f, -length / 2.0f),
      -Vector3.I, -Vector3.J,
      displace, color)

    val right = Plane(length, height,
      origin + Vector3(width / 2.0f, height / 2.0f, 0.0f),
      -Vector3.K, -Vector3.J,
      displace, color)

    val bottom = Plane(width, length,
      origin,
      -Vector3.I, Vector3.K,
      displace, color)

    val top = Plane(width, length,
      origin + Vector3(0.0f, height, 0.0f),
      Vector3.I, Vector3.K,
      displace, color)

    Vector(front, left, back, right, bottom, top)
  }

}
