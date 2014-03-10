package edu.utexas.cs.sdao.reyes.anim

import edu.utexas.cs.sdao.reyes.core.Vector3

/**
 * An animatable parameter that just returns a constant, static value.
 *
 * Use this as a wrapper around values when you want to use non-animating
 * parameters in a context that requires an `Animatable[T]`.
 *
 * Note that some types, such as floats and `Vector3`s, are implicitly
 * converted to a static when necessary if [[edu.utexas.cs.sdao.reyes.anim.Static]]._
 * is imported.
 *
 * @param x the static value
 * @tparam T the type of the parameter
 */
class Static[T](x: T) extends Animatable[T] {

  /**
   * Gets the value of the parameter at the current time.
   * @return the current value of the parameter
   */
  override def apply(): T = x

}

/**
 * Utilities for using static values in animatable parameters.
 */
object Static {

  def apply[T](x: T): Static[T] = new Static(x)

  /**
   * A static float value parameter.
   * @param x the static value
   */
  implicit class StaticFloat(x: Float) extends Animatable[Float] {

    /**
     * Gets the value of the parameter at the given time.
     * @return the value of the parameter
     */
    override def apply(): Float = x

  }

  /**
   * A static Vector3 value parameter.
   * @param x the static value
   */
  implicit class StaticVector3(x: Vector3) extends Animatable[Vector3] {

    /**
     * Gets the value of the parameter at the given time.
     * @return the value of the parameter
     */
    override def apply(): Vector3 = x

  }

  /**
   * A static vector (collection) parameter.
   * @param x the static value
   */
  implicit class StaticVector[T](x: Vector[T]) extends Animatable[Vector[T]] {

    /**
     * Gets the value of the parameter at the current time.
     * @return the current value of the parameter
     */
    override def apply(): Vector[T] = x

  }

}
