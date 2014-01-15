package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core._

/**
 * A grid of micropolygons on a UV surface.
 * @param width the number of U segments
 * @param height the number of V segments
 * @param data an array of tuples, the first element being the vertex and the second being the normal;
 *             this array is arranged in U-major order, i.e. all elements with the same U-index are contiguous.
 */
class MicropolygonGrid(width: Int,
                       height: Int,
                       data: Array[(Vector3, Vector3)]) {

  if (data.length != width * height) {
    throw new IllegalArgumentException("length of data array != width * height")
  }

  private def idx(u: Int, v: Int) = u * width + v

  def getVertex(u: Int, v: Int) = data(idx(u, v))._1

  def getNormal(u: Int, v: Int) = data(idx(u, v))._2

  /**
   * Projects the grid onto the screen.
   */
  def project(cam: Camera): ProjectedMicropolygonGrid = {
    new ProjectedMicropolygonGrid(
      width,
      height,
      data.map(x => {
        val vtx = x._1
        val norm = x._2
        val proj = cam.project(vtx)
        (Vector3(proj.x, proj.y, vtx.z), norm)
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

}
