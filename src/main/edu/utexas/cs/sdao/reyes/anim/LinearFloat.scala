package edu.utexas.cs.sdao.reyes.anim

/**
 * A float parameter that changes linearly over time.
 * Note that the value is interpolated even before the starting time
 * and after the ending time; the values are not clamped to
 * the starting and ending values outside of the given range.
 *
 * @param startVal the value returned at the `startTime`
 * @param endVal the value returned at the `endTime`
 * @param startTime the starting time
 * @param endTime the ending time; must be after the ending time.
 * @param timeline the linked timeline on which this parameter changes
 */
case class LinearFloat(startVal: Float,
                  endVal: Float,
                  startTime: Float,
                  endTime: Float,
                  timeline: Timeline)
  extends Animatable[Float] {

  private val timeSpan = endTime - startTime

  if (timeSpan <= 0.0f) throw new IllegalArgumentException("endTime must occur after startTime")

  /**
   * Gets the value of the parameter at the current time.
   * @return the value of the parameter
   */
  override def apply(): Float = {
    val t = timeline.time
    val tNorm = (t - startTime) / timeSpan
    (1.0f - tNorm) * startVal + tNorm * endVal
  }

}
