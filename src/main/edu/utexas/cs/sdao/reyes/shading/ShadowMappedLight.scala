package edu.utexas.cs.sdao.reyes.shading

import edu.utexas.cs.sdao.reyes.graph.SurfaceNode

/**
 * The base class for lights that support shadow-mapping.
 */
abstract class ShadowMappedLight(attenuateConst: Float = 0.3f,
                                 attenuateLin: Float = 0.6f,
                                 attenuateQuad: Float = 0.1f)
  extends Light(attenuateConst, attenuateLin, attenuateQuad) {

  /**
   * Renders the shadow map for the given objects.
   * Remove objects from the list of surfaces if you wish for them
   * not to cast any shadows.
   * If this function is never called before final rendering,
   * no shadows will be generated for this light.
   * @param root the root node of the hierarchy to render shadows for
   */
  def renderShadowMap(root: SurfaceNode)

}
