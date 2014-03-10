package edu.utexas.cs.sdao.reyes.anim

import edu.utexas.cs.sdao.reyes.core.{Vector3, Matrix4}
import edu.utexas.cs.sdao.reyes.anim.Static._

/**
 * A parameter that outputs transformation matrices that change over time.
 * This parameter calculates a transformation matrix that scales first,
 * then rotates, then translates. (In matrix terms, the resultant
 * transformation matrix is the translation matrix, multiplied by the rotation
 * matrix, then multiplied by the scaling matrix, in that order.)
 */
case class AnimatableTransform(translation: Animatable[Vector3] = Vector3.ZERO,
                          rotation: Animatable[Vector3] = Vector3.ZERO,
                          scale: Animatable[Vector3] = Vector3.ONE)
  extends Animatable[Matrix4] {

  /**
   * Gets the value of the parameter at the current time.
   * @return the current value of the parameter
   */
  override def apply(): Matrix4 = {
    val curTrans = translation()
    val curRot = rotation()
    val curScale = scale()

    Matrix4.scale(curScale).rotate(curRot).translate(curTrans)
  }

}
