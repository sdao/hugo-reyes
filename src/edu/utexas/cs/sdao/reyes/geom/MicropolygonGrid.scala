package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core._

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

  private def idx(u: Int, v: Int) = u * height + v

  def getVertex(u: Int, v: Int) = data(idx(u, v))._1

  def getNormal(u: Int, v: Int) = data(idx(u, v))._2

  def getUV(u: Int, v: Int) = data(idx(u, v))._3

  def getColor(u: Int, v: Int) = data(idx(u, v))._4

  /**
   * Projects the grid onto the screen.
   * Note that, in the projected grid, the vertex will have three components:
   * x and y are the screen coordinates, and z is the z-depth for z-buffer calculations.
   */
  def project(cam: Camera): ProjectedMicropolygonGrid = {
    new ProjectedMicropolygonGrid(
      width,
      height,
      surface,
      data.map(x => {
        val vtx = x._1
        val norm = x._2
        val uv = x._3
        val color = x._4
        val proj = cam.project(vtx)
        (Vector3(proj.x, proj.y, vtx.z), norm, uv, color)
      }),
      cam
    )
  }

  /**
   * Determines the bounding box of the grid.
   * @return the bounding box
   */
  def boundingBox: BoundingBox = {
    data.foldLeft(BoundingBox.empty)((accum, cur) => accum.expand(cur._1))
  }

  /**
   * Performs shading routines on the micropolygon grid and returns a new grid.
   * @param displacementMap a function that maps a vertex, a normal, and a UV coordinate to
   *                        a new vertex and a new normal
   * @param colorMap a function that maps a UV coordinate to a surface color
   * @return a new micropolygon grid
   */
  def shade(displacementMap: (Vector3, Vector3, Vector2) => (Vector3, Vector3), colorMap: Vector2 => Color): MicropolygonGrid = {
    val newData = data.map(x => {
      val displacement = displacementMap(x._1, x._2, x._3)
      val vtx = displacement._1
      val norm = displacement._2
      val color = colorMap(x._3)
      (vtx, norm, x._3, color)
    })

    // TODO: allow normal recomputation.

    new MicropolygonGrid(width, height, surface, newData)
  }

}
