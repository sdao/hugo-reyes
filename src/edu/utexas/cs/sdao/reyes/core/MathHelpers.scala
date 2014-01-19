package edu.utexas.cs.sdao.reyes.core

import math._

object MathHelpers {

  def limit(lower: Float, upper: Float, x: Float): Float = max(lower, min(upper, x))

  def limit(lower: Int, upper: Int, x: Int): Int = max(lower, min(upper, x))

}
