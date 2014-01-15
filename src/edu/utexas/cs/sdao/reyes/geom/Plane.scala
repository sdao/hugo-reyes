package edu.utexas.cs.sdao.reyes.geom

import edu.utexas.cs.sdao.reyes.core.{Vector3, FilledBoundingBox}

case class Plane(width: Float, length: Float, origin: Vector3, faceNegative: Boolean) extends Surface {

  def boundingBox: FilledBoundingBox =
    FilledBoundingBox(Vector3(origin.x - width / 2.0f, origin.y - length / 2.0f, origin.z),
      Vector3(origin.x + width / 2.0f, origin.y + length / 2.0f, origin.z))

  /**
   * Gets the world coordinates at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world coordinates
   */
  def getVertex(u: Float, v: Float): Vector3 = {
    boundingBox.lowBound + Vector3(u * width, v * length, 0.0f)
  }

  /**
   * Gets the world-space normal at a certain UV coordinate.
   * @param u the U component
   * @param v the V component
   * @return the world-space normal
   */
  def getNormal(u: Float, v: Float): Vector3 = {
    if (faceNegative)
      Vector3(0.0f, 0.0f, -1.0f)
    else
      Vector3(0.0f, 0.0f, 1.0f)
  }
}
