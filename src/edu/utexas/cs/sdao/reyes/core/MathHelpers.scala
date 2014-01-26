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

  /**
   * Returns 0 if the value is below the minimum and 1 if the value is above the maximum.
   * Otherwise, it returns a smooth Hermite interpolation between the min and max.
   *
   * See [[http://http.developer.nvidia.com/CgTutorial/cg_tutorial_chapter05.html this Nvidia page]]
   * for more info.
   * @param low the minimum value
   * @param high the maximum value
   * @param x the value to smoothstep interpolate
   * @return the smoothstep-interpolated value
   */
  def smoothstep(low: Float, high: Float, x: Float): Float = {
    if (x <= low) 0.0f
    else if (x >= high) 1.0f
    else {
      (-2.0f * pow((x - low)/(high-low), 3.0f) +
        3.0f * pow((x - low)/(high-low), 2.0f)).toFloat
    }
  }

}
