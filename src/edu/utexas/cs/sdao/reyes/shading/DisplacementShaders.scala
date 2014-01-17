package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Vector2, Vector3}
import math._

object DisplacementShaders {

  type DisplacementShader = (Vector3, Vector3, Vector2) => Vector3

  def noDisplace: DisplacementShader = {
    (vtx, normal, uv) => vtx
  }

  def shellDisplace(dist: Float): DisplacementShader = {
    (vtx, normal, uv) => vtx + normal * dist
  }

  def checkerDisplace(dist1: Float, dist2: Float): DisplacementShader = {
    (vtx, normal, uv) => {
      val uOff = (uv.x * 20.0f).toInt
      val vOff = (uv.y * 10.0f).toInt

      if (uOff % 2 == vOff % 2)
        vtx + normal * dist1
      else
        vtx + normal * dist2
    }
  }

  def bumpyDisplace: DisplacementShader = {
    (vtx, normal, uv) => {
      val disp = min(0.5, sin(uv.y * 100.0) * cos(uv.x * 100.0) * 0.1).toFloat
      vtx - normal * disp
    }
  }

  val DEFAULT = noDisplace

}
