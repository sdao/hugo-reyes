package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.geom.{SplitSurface, Surface}
import scala.collection.mutable
import edu.utexas.cs.sdao.reyes.core.Camera

object Renderer {

  def split(surfaces: Iterable[Surface], cam: Camera): List[SplitSurface] = {
    val queue = mutable.Queue[SplitSurface]()
    for (x <- surfaces) {
      queue.enqueue(x.toSplitSurface)
    }

    val done = mutable.MutableList[SplitSurface]()

    while (queue.nonEmpty) {
      val surface = queue.dequeue()
      val grid = surface.dice().project(cam)

      if (grid.isVisible) {
        grid.isSplittable match {
          case Some(dir) => queue ++= surface.split(dir)
          case None => done += surface
        }
      }
    }

    done.toList
  }

  def debugVerifySplit(splitSurfaces: Iterable[SplitSurface]): Unit = {
    splitSurfaces.groupBy(x => x.surface).map(group => {
      println(s"Surface: ${group._1}")

      val sorted = group._2.toVector.sortBy(x => (x.startU, x.startV))
      sorted.map(x => println(s"${x.startU} < u < ${x.endU}; ${x.startV} < v < ${x.endV}"))

      val remain = sorted.foldLeft(0.0)((accum, cur) => accum + (cur.endU - cur.startU) * (cur.endV - cur.startV))
      println(s"Remaining: $remain")
    })
  }

}
