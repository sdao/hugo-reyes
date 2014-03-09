package edu.utexas.cs.sdao.reyes.anim

/**
 * A parameter that can change over time.
 * @tparam T the type of the parameter
 */
trait Animatable[T] {

  /**
   * Gets the value of the parameter at the current time.
   * @return the current value of the parameter
   */
  def apply(): T

}
