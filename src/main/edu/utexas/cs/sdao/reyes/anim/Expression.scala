package edu.utexas.cs.sdao.reyes.anim

/**
 * A parameter that takes the value from another animatable parameter
 * and transforms it. For example, you can use this parameter
 * to halve another parameter as it is animating.
 */
case class Expression[T](param: Animatable[T],
                         expr: T => T)
  extends Animatable[T] {

  /**
   * Gets the value of the parameter at the current time.
   * @return the current value of the parameter
   */
  override def apply(): T = expr(param())

}
