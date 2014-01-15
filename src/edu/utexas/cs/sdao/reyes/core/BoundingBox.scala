package edu.utexas.cs.sdao.reyes.core

import math._

case class BoundingBox(lowBound: Vector3 = Vector3.ZERO, upBound: Vector3 = Vector3.ZERO) {

  def expand(pt: Vector3): BoundingBox =
    BoundingBox(
      Vector3(min(lowBound.x, pt.x), min(lowBound.y, pt.y), min(lowBound.z, pt.z)),
      Vector3(max(upBound.x, pt.x), max(upBound.y, pt.y), max(upBound.z, pt.z))
    )

}
