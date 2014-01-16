package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Vector2, Vector3}
import math._

object DisplacementShaders {

  def noDisplace: (Vector3, Vector3, Vector2) => (Vector3, Vector3) = {
    (vtx, normal, uv) => (vtx, normal)
  }

  def shellDisplace(dist: Float): (Vector3, Vector3, Vector2) => (Vector3, Vector3) = {
    (vtx, normal, uv) => (vtx + normal * dist, normal)
  }

  def checkerDisplace(dist1: Float, dist2: Float): (Vector3, Vector3, Vector2) => (Vector3, Vector3) = {
    (vtx, normal, uv) => {
      val uOff = (uv.x * 20.0f).toInt
      val vOff = (uv.y * 10.0f).toInt

      if (uOff % 2 == vOff % 2)
        (vtx + normal * dist1, normal)
      else
        (vtx + normal * dist2, normal)
    }
  }

  def bumpyDisplace: (Vector3, Vector3, Vector2) => (Vector3, Vector3) = {
    (vtx, normal, uv) => {
      val disp = min(0.5, sin(uv.y * 100.0) * cos(uv.x * 100.0) * 0.1).toFloat
      (vtx - normal * disp, normal)
    }
  }

}
