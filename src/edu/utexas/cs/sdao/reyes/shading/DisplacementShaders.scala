package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Vector2, Vector3}

object DisplacementShaders {

  def noDisplace: (Vector3, Vector3, Vector2) => (Vector3, Vector3) = {
    (vtx, normal, uv) => (vtx, normal)
  }

  def shellDisplace(dist: Float): (Vector3, Vector3, Vector2) => (Vector3, Vector3) = {
    (vtx, normal, uv) => (vtx + normal * dist, normal)
  }

}
