package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{FilledBoundingBox, Vector3, BoundingBox}
import edu.utexas.cs.sdao.reyes.shading.DisplacementShaders.DisplacementShader
import edu.utexas.cs.sdao.reyes.shading.ColorShaders.ColorShader
import edu.utexas.cs.sdao.reyes.shading.{ColorShaders, DisplacementShaders}

/**
 * A surface parameterized over UV coordinates.
 */
abstract class Surface(val displacementShader: DisplacementShader = DisplacementShaders.DEFAULT,
                       val colorShader: ColorShader = ColorShaders.DEFAULT) {

  /**
   * Calculates the bounding box.
   * @return A box containing the bounds of the surface.
   */
  def boundingBox: FilledBoundingBox

  /**
   * Creates a new split surface with this surface, unsplit,
   * with the split count equal to 0.
   * @return A SplitSurface containing this surface.
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

}
