package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.core._
import edu.utexas.cs.sdao.reyes.geom.SplitSurface

/**
 * A grid of micropolygons on a UV surface.
 * @param width the number of U segments
 * @param height the number of V segments
 * @param surface the original surface grom which the grid was derived
 * @param data an array of tuples, the first element being the vertex, the second being the normal,
 *             the third being the UV coordinate and the last being the color;
 *             this array is arranged in U-major order, i.e. all elements with the same U-index are contiguous.
 */
class MicropolygonGrid(width: Int,
                       height: Int,
                       surface: SplitSurface,
                       data: Array[(Vector3, Vector3, Vector2, Color)]) {

  if (data.length != width * height) {
    throw new IllegalArgumentException("length of data array != width * height")
  }

  private lazy val bounds = data.foldLeft(EmptyBoundingBox: BoundingBox)((accum, cur) => accum.expand(cur._1))

  private def idx(u: Int, v: Int) = u * height + v

  /**
   * Gets the world-space coordinate of a vertex.
   * @param u the U-index of the vertex
   * @param v the V-index of the vertex
   * @return the vertex at the UV index
   */
  def getVertex(u: Int, v: Int) = data(idx(u, v))._1

  /**
   * Gets the world-space normal of a vertex.
   * @param u the U-index of the vertex
   * @param v the V-index of the vertex
   * @return the normal at the UV index
   */
  def getNormal(u: Int, v: Int) = data(idx(u, v))._2

  /**
   * Gets the UV coordinate of a vertex.
   * @param u the U-index of the vertex
   * @param v the V-index of the vertex
   * @return the UV coordinate at the UV index
   */
  def getUV(u: Int, v: Int) = data(idx(u, v))._3

  /**
   * Gets the color of a vertex.
   * Typically, the color will be Color.BLACK unless the grid has been shaded.
   * @param u the U-index of the vertex
   * @param v the V-index of the vertex
   * @return the color at the UV index
   */
  def getColor(u: Int, v: Int) = data(idx(u, v))._4

  /**
   * Projects the grid onto the screen.
   * Note that, in the projected grid, the vertex will have three components:
   * x and y are the screen coordinates, and z is the z-depth for z-buffer calculations.
   */
  def project(proj: Projection): ProjectedMicropolygonGrid = {
    new ProjectedMicropolygonGrid(
      width,
      height,
      surface,
      data.map(x => {
        val vtx = x._1
        val norm = x._2
        val uv = x._3
        val color = x._4
        val projVtx = proj.projectToScreen(vtx)
        (projVtx, norm, uv, color)
      }),
      proj
    )
  }

  /**
   * The bounding box of the grid.
   * @return the bounding box
   */
  def boundingBox: BoundingBox = bounds

  /**
   * Performs shading routines on the micropolygon grid and returns a new grid.
   * The grid will be displaced and colored. Normals will be recalculated
   * after displacement.
   * @param eyeProjection the projection from world coordinates to the eye
   * @param displaceOnly whether to only displace the grid without calculating colors
   * @return a new micropolygon grid
   */
  def shade(eyeProjection: Projection,
            displaceOnly: Boolean = false): MicropolygonGrid = {
    val displacedData = data.map(x => {
      val displacement = surface.surface.displacementShader(x._1, x._2, x._3, eyeProjection)
      (displacement, x._2, x._3)
    })

    val fixedNormalsData = recalculateNormals(displacedData)

    val coloredData =
      if (displaceOnly)
        fixedNormalsData.map(x => {
          (x._1, x._2, x._3, Color.BLACK)
        })
      else
        fixedNormalsData.map(x => {
          val color = surface.surface.colorShader(x._1, x._2, x._3, eyeProjection)
          (x._1, x._2, x._3, color)
        })

    new MicropolygonGrid(width, height, surface, coloredData)
  }

  private def recalculateNormals(oldData: Array[(Vector3, Vector3, Vector2)]):
    Array[(Vector3, Vector3, Vector2)] = {
    (0 until width).flatMap(u => {
      (0 until height).map(v => {
        val old = oldData(idx(u, v))
        val newNormal =
          if (u < width - 1 && v < height - 1) {
            val v1 = oldData(idx(u, v))._1 - oldData(idx(u + 1, v))._1
            val v2 = oldData(idx(u, v))._1 - oldData(idx(u, v + 1))._1
            (v1 cross v2).normalize
          } else old._2

        (old._1, newNormal, old._3)
      })
    }).toArray
  }

}

object MicropolygonGrid {
  /**
   * The amount of dicing to do when testing grid bounds during the splitting phase.
   */
  val INITIAL_DICE_COUNT = 8
}
