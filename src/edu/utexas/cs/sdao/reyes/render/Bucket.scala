package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.core.{Color, Vector2, Camera}

/**
 * Created by Steve on 17/01/2014.
 */
case class Bucket(xMin: Int, yMin: Int,
                  xMax: Int, yMax: Int,
                  objects: Iterable[PipelineInfo],
                  cam: Camera) {

  def render() = {
    val projected = objects.par.map(obj => {
      obj.dice.shade().project(cam)
    })

    val data = Array.ofDim[Int](xMax - xMin, yMax - yMin)
    for (i <- 0 until (xMax - xMin)) {
      for (j <- 0 until (yMax - yMin)) {
        val isects = projected.flatMap(_.contains(Vector2(i + xMin, j + yMin)))
        if (isects.nonEmpty) {
          data(i)(j) = isects.maxBy(_._1)._2.clamp.rgb
        }
      }
    }

    cam.render((img) => {
      for (i <- 0 until (xMax - xMin)) {
        for (j <- 0 until (yMax - yMin)) {
          img.setRGB(i + xMin, cam.height - (j + yMin) - 1, data(i)(j))
        }
      }
    })
  }

}
