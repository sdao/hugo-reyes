package edu.utexas.cs.sdao.reyes.anim

import edu.utexas.cs.sdao.reyes.core.Vector3
import edu.utexas.cs.sdao.reyes.anim.StaticParams._

/**
 * A Vector3 parameter whose individual components can change over time.
 */
case class AnimatableVector3(x: Animatable[Float] = 0.0f,
                        y: Animatable[Float] = 0.0f,
                        z: Animatable[Float] = 0.0f)
  extends Animatable[Vector3] {

  /**
   * Gets the value of the parameter at the current time.
   * @return the current value of the parameter
   */
  override def apply(): Vector3 = Vector3(x(), y(), z())

}
