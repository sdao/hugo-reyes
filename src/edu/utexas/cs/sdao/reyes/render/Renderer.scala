package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.geom.{DiceInfo, SplitSurface, Surface}
import scala.collection.mutable
import edu.utexas.cs.sdao.reyes.core.{Color, Camera}
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}
import java.awt.image.BufferedImage

object Renderer {

  def split(surfaces: Iterable[Surface], cam: Camera): List[DiceInfo] = {
    val queue = mutable.Queue[SplitSurface]()
    for (x <- surfaces) {
      // TODO: only enqueue surfaces whose bounding boxes intersect the camera frustum.
      queue.enqueue(x.toSplitSurface)
    }

    val done = mutable.MutableList[DiceInfo]()

    while (queue.nonEmpty) {
      val surface = queue.dequeue()
      val grid = surface.dice().project(cam)

      if (grid.isVisible) {
        grid.isSplittable match {
          case Some(dir) => queue ++= grid.split(dir)
          case None => done += grid.diceInfo
        }
      }
    }

    done.toList
  }

  def debugVerifySplit(diceInfos: Iterable[DiceInfo]): Unit =
    debugVerifySplitSurfaces(diceInfos.map(_.surface))

  def debugVerifySplitSurfaces(splitSurfaces: Iterable[SplitSurface]): Unit = {
    splitSurfaces.groupBy(x => x.surface).map(group => {
      println(s"Surface: ${group._1}")

      val sorted = group._2.toVector.sortBy(x => (x.startU, x.startV))
      sorted.map(x => println(s"${x.startU} < u < ${x.endU}; ${x.startV} < v < ${x.endV}"))

      val remain = sorted.foldLeft(0.0)((accum, cur) => accum + (cur.endU - cur.startU) * (cur.endV - cur.startV))
      println(s"Remaining: $remain")
    })
  }

  def render(diceInfos: Iterable[DiceInfo], cam: Camera) {
    diceInfos.map(diceInfo => {
      println(s"Rendering $diceInfo")
      val dicedGrid = diceInfo.dice
      val shadedGrid = dicedGrid.shade(DisplacementShaders.noDisplace, ColorShaders.solid(Color.RED))
      val projectedGrid = shadedGrid.project(cam)
      cam.render((buffer, zBuffer) => projectedGrid.rasterize(buffer, zBuffer))
    })
  }

}
