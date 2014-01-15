package edu.utexas.cs.sdao.reyes.core

import math._

abstract class BoundingBox {
  def expand(pt: Vector3): BoundingBox
}

object BoundingBox {

  def empty: BoundingBox = EmptyBoundingBox()

}

case class EmptyBoundingBox() extends BoundingBox {

  def expand(pt: Vector3): BoundingBox = {
    FilledBoundingBox(pt, pt)
  }

}

case class FilledBoundingBox(lowBound: Vector3 = Vector3.ZERO,
                             upBound: Vector3 = Vector3.ZERO) extends BoundingBox {

  def expand(pt: Vector3): BoundingBox =
    FilledBoundingBox(
      Vector3(min(lowBound.x, pt.x), min(lowBound.y, pt.y), min(lowBound.z, pt.z)),
      Vector3(max(upBound.x, pt.x), max(upBound.y, pt.y), max(upBound.z, pt.z))
    )

}
