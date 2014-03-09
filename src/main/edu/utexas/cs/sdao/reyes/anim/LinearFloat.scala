package edu.utexas.cs.sdao.reyes.anim

/**
 * A float parameter that changes linearly over time.
 */
case class LinearFloat(startVal: Float,
                  endVal: Float,
                  startTime: Float,
                  endTime: Float,
                  clock: Timeline)
  extends Animatable[Float] {

  val timeSpan = endTime - startTime

  if (timeSpan <= 0.0f) throw new IllegalArgumentException("endTime must occur after startTime")

  /**
   * Gets the value of the parameter at the current time.
   * @return the value of the parameter
   */
  override def apply(): Float = {
    val t = clock.time
    val tNorm = (t - startTime) / timeSpan
    (1.0f - tNorm) * startVal + tNorm * endVal
  }

}
