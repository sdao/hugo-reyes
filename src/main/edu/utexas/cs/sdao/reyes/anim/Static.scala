package edu.utexas.cs.sdao.reyes.anim

import edu.utexas.cs.sdao.reyes.core.Vector3

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

  implicit class StaticVector(x: Vector3) extends Animatable[Vector3] {

    /**
     * Gets the value of the parameter at the given time.
     * @return the value of the parameter
     */
    override def apply(): Vector3 = x

  }

}
