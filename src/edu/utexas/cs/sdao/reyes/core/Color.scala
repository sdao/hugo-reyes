package edu.utexas.cs.sdao.reyes.core

import scala.math._

/**
 * Defines a floating-point color in the RGB color space,
 * with 32-bit precision for each component.
 *
 * Each component should be in the range 0..1.
 */
case class Color(r: Float, g: Float, b: Float) {

  def +(right: Color): Color = {
    Color(r + right.r, g + right.g, b + right.b)
  }

  def +(right: Float): Color = {
    Color(r + right, g + right, b + right)
  }

  def -(right: Color): Color = {
    Color(r - right.r, g - right.g, b - right.b)
  }

  def -(right: Float): Color = {
    Color(r - right, g - right, b - right)
  }

  def *(right: Color): Color = {
    Color(r * right.r, g * right.g, b * right.b)
  }

  def *(right: Float): Color = {
    Color(r * right, g * right, b * right)
  }

  def /(right: Color): Color = {
    Color(r / right.r, g / right.g, b / right.b)
  }

  def /(right: Float): Color = {
    Color(r / right, g / right, b / right)
  }

  def ^(right: Float): Color = {
    Color(pow(r, right).toFloat, pow(g, right).toFloat, pow(b, right).toFloat)
  }

  /**
   * Adjusts any component that is greater than 1.0 down to 1.0.
   */
  def clamp: Color = {
    Color(max(min(r, 1.0f), 0.0f), max(min(g, 1.0f), 0.0f), max(min(b, 1.0f), 0.0f))
  }

  def rgb: Int = {
    new java.awt.Color(r, g, b).getRGB
  }

}

object Color {
  val BLACK = Color(0.0f, 0.0f, 0.0f)
  val WHITE = Color(1.0f, 1.0f, 1.0f)
  val RED = Color(1.0f, 0.0f, 0.0f)
  val GREEN = Color(0.0f, 0.8f, 0.2f)
}
