package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Vector2, Vector3}
import math._
import edu.utexas.cs.sdao.reyes.render.Projection

object DisplacementShaders {

  /**
   * A function that takes:
   * (1) the point of the vertex,
   * (2) the normal at the vertex,
   * (3) the UV coordinate of the vertex, and
   * (4) the projection from world coordinates to the eye,
   * returning a new, displaced vertex.
   */
  type DisplacementShader = (Vector3, Vector3, Vector2, Projection) => Vector3

  /**
   * Generates a displacement shader that does nothing.
   * @return a null displacement shader
   */
  def noDisplace: DisplacementShader = {
    (vtx, normal, uv, proj) => vtx
  }

  /**
   * Generates a displacement shader that displaces all vertices
   * uniformly along their normals.
   * @param dist the distance to displace
   * @return a shell shader
   */
  def shellDisplace(dist: Float): DisplacementShader = {
    (vtx, normal, uv, proj) => vtx + normal * dist
  }

  /**
   * Generates a shader that shell-displaces using a checkerboard pattern.
   * @param dist1 the distance to displace along the black squares
   * @param dist2 the distance to displace along the white squares
   * @return a checkerboard shader
   */
  def checkerDisplace(dist1: Float, dist2: Float): DisplacementShader = {
    (vtx, normal, uv, proj) => {
      val uOff = (uv.x * 20.0f).toInt
      val vOff = (uv.y * 10.0f).toInt

      if (uOff % 2 == vOff % 2)
        vtx + normal * dist1
      else
        vtx + normal * dist2
    }
  }

  /**
   * Generates a shader that produces a bumpy pattern, like a toy ball.
   * @return a bumpy shader
   */
  def bumpyDisplace: DisplacementShader = {
    (vtx, normal, uv, proj) => {
      val disp = min(0.5, sin(uv.y * 100.0) * cos(uv.x * 100.0) * 0.1).toFloat
      vtx - normal * disp
    }
  }

  val DEFAULT = noDisplace

}
