package edu.utexas.cs.sdao.reyes.anim

/**
 * Utilities for using static values in animatable parameters.
 * Import this package to implicitly cast any object into a
 * wrapped version for use in animation contexts that require an
 * `Animatable`.
 */
object Static {

  /**
   * Creates a StaticAny[T] from the given object of type T.
   * @param x the static value
   * @tparam T the type of the parameter
   * @return the wrapped value
   */
  def apply[T](x: T): StaticAny[T] = new StaticAny(x)

  /**
   * An animatable parameter that just returns a constant, static value.
   *
   * Use this as a wrapper around values when you want to use non-animating
   * parameters in any context that requires an `Animatable[T]`.
   *
   * @param x the static value
   * @tparam T the type of the parameter
   */
  implicit class StaticAny[T](x: T) extends Animatable[T] {

    /**
     * Gets the value of the parameter at the current time.
     * @return the current value of the parameter
     */
    override def apply(): T = x

  }

}
