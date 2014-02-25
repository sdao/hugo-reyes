package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{BoundingSphere, Matrix4, Vector3}
import edu.utexas.cs.sdao.reyes.shading.DisplacementShaders.DisplacementShader
import edu.utexas.cs.sdao.reyes.shading.ColorShaders.ColorShader
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}

/**
 * A surface parameterized over UV coordinates.
 */
abstract class Surface(val displacementShader: DisplacementShader = DisplacementShaders.DEFAULT,
                       val colorShader: ColorShader = ColorShaders.DEFAULT) {

  /**
   * The bounding sphere of the surface.
   * The bounding sphere can be larger than the surface, but must not be smaller.
   * @return a sphere containing the bounds of the surface
   */
  def boundingSphere: BoundingSphere

  /**
   * Creates a new split surface with this surface, unsplit,
   * with the split count equal to 0.
   * @return a split surface containing this surface.
   */
  def toSplitSurface: SplitSurface = SplitSurface(this)

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  def getVertex(u: Float, v: Float): Vector3

  /**
   * Gets the world-space normal at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world-space normal
   */
  def getNormal(u: Float, v: Float): Vector3

  def transform(newTransform: Matrix4): TransformedSurface = {
    new TransformedSurface(this, newTransform)
  }

}
