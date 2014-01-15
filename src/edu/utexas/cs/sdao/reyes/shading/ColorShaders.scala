package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Color, Vector2}

object ColorShaders {

  def solid(c: Color): Vector2 => Color = {
    (uv) => c
  }

  def checker(c: Color, d: Color): Vector2 => Color = {
    (uv) => {
      val uOff = (uv.x * 100.0f).toInt
      val vOff = (uv.y * 100.0f).toInt

      if (uOff % 2 == vOff % 2)
        c
      else
        d
    }
  }

}
