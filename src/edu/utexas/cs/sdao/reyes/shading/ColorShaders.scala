package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.core.{Color, Vector2}

object ColorShaders {

  def solid(c: Color): Vector2 => Color = {
    (uv) => c
  }

}
