package edu.utexas.cs.sdao.reyes.anim

import edu.utexas.cs.sdao.reyes.core.Vector3

/**
 * Utilities for using static values in animatable parameters.
 */
object StaticParams {

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
