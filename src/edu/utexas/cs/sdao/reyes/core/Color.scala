package edu.utexas.cs.sdao.reyes.core

import scala.math._

/**
 * Defines a floating-point color in the RGB color space,
 * with 32-bit precision for each component.
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
    Color(min(r, 1.0f), min(g, 1.0f), min(b, 1.0f))
  }

}
