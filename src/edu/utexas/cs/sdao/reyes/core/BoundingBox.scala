package edu.utexas.cs.sdao.reyes.core

import math._

abstract class BoundingBox {
  def expand(pt: Vector3): FilledBoundingBox
  def contains2D(pt: Vector2): Boolean
  def intersects2D(b: BoundingBox): Boolean
}

object BoundingBox {

  def empty: BoundingBox = EmptyBoundingBox()

}

case class EmptyBoundingBox() extends BoundingBox {

  def expand(pt: Vector3): FilledBoundingBox = {
    FilledBoundingBox(pt, pt)
  }

  def contains2D(pt: Vector2): Boolean = false

  def intersects2D(b: BoundingBox): Boolean = false

}

case class FilledBoundingBox(lowBound: Vector3 = Vector3.ZERO,
                             upBound: Vector3 = Vector3.ZERO) extends BoundingBox {

  def expand(pt: Vector3): FilledBoundingBox =
    FilledBoundingBox(
      Vector3(min(lowBound.x, pt.x), min(lowBound.y, pt.y), min(lowBound.z, pt.z)),
      Vector3(max(upBound.x, pt.x), max(upBound.y, pt.y), max(upBound.z, pt.z))
    )

  def contains2D(pt: Vector2): Boolean = {
    pt.x >= lowBound.x && pt.x <= upBound.x &&
      pt.y >= lowBound.y && pt.y <= upBound.y
  }

  def intersects2D(b: BoundingBox): Boolean = {
    b match {
      case EmptyBoundingBox() => false
      case FilledBoundingBox(bLow, bUp) => {
        val low2 = lowBound.truncateToVector2
        val up2 = upBound.truncateToVector2
        val bLow2 = bLow.truncateToVector2
        val bUp2 = bUp.truncateToVector2

        (bLow2 >= low2 && bLow2 <= up2) ||
          (bUp2 >= low2 && bLow2 <= up2) ||
          (bLow2 <= low2 && bUp2 >= up2)
      }
    }
  }

}
