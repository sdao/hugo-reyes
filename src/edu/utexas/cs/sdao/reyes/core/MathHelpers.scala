package edu.utexas.cs.sdao.reyes.core

import math._

/**
 * Helpers for performing math.
 */
object MathHelpers {

  /**
   * Returns x if x is between lower and upper,
   * lower if x is less than lower, and
   * upper if x is greater than upper.
   * @param lower the lower bound
   * @param upper the upper bound
   * @param x the value to clamp
   * @return the clamped value
   */
  def clamp(lower: Float, upper: Float, x: Float): Float = max(lower, min(upper, x))

  /**
   * Returns x if x is between lower and upper,
   * lower if x is less than lower, and
   * upper if x is greater than upper.
   * @param lower the lower bound
   * @param upper the upper bound
   * @param x the value to clamp
   * @return the clamped value
   */
  def clamp(lower: Int, upper: Int, x: Int): Int = max(lower, min(upper, x))

  /**
   * A shortcut for clamp(0.0f, 1.0f, x).
   * @param x the value to clamp
   * @return the clamped value
   */
  def clampUnit(x: Float) = max(0.0f, min(1.0f, x))

}
