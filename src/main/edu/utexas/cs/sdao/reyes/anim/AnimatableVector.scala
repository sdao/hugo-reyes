package edu.utexas.cs.sdao.reyes.anim

/**
 * A Vector (collection) parameter whose individual elements can change over time.
 */
class AnimatableVector[T](params: Vector[Animatable[T]])
  extends Animatable[Vector[T]] {

  /**
   * Gets the value of the parameter at the current time.
   * @return the current value of the parameter
   */
  override def apply(): Vector[T] = params.map(_())

}
