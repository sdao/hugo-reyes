package edu.utexas.cs.sdao.reyes.render

import scala.math._
import edu.utexas.cs.sdao.reyes.core._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingBox
import edu.utexas.cs.sdao.reyes.core.EmptyBoundingBox

/**
 * Projects world coordinates into screen coordinates.
 * Does not contain any apparatus for rendering images; for that functionality,
 * see [[edu.utexas.cs.sdao.reyes.render.Camera Camera]].
 *
 * @param worldToCamera a transformation matrix that converts world coordinates
 *                      to camera coordinates; e.g., if the camera is translated one unit
 *                      to the left, then this matrix will move world objects one unit to
 *                      the right
 * @param fieldOfView the camera's horizontal field of view in radians,
 *                    measured as the angle from the left of the visible screen to the right;
 *                    acceptable values are 0 < fieldOfView < Pi
 * @param width the width of the output image plane
 * @param height the height of the output image plane
 */
class Projection(worldToCamera: Matrix4 = Matrix4.IDENTITY,
                 fieldOfView: Float = toRadians(60.0).toFloat,
                 width: Int = 800,
                 height: Int = 600) {

  if (fieldOfView <= 0.0 || fieldOfView >= Pi)
    throw  new IllegalArgumentException("fieldOfView out of range")

  val focalLength = (width / 2.0f) / tan(fieldOfView / 2.0f).toFloat // Trigonometry, what's that?!
  private val halfWidth = width / 2.0f
  private val halfHeight = height / 2.0f

  /**
   * Projects a point into the image space defined by u=0..width and v=0..height.
   * The z-component is the z-depth of the point.
   * @param u the vector to project
   * @return the projected vector
   */
  def project(u: Vector3): Vector3 = {
    val v = worldToCamera * u
    if (v.z == 0.0f)
      Vector3(focalLength * v.x / Float.MinPositiveValue + halfWidth,
        focalLength * v.y / Float.MinPositiveValue + halfHeight,
        v.z)
    else
      Vector3(focalLength * v.x / -v.z + halfWidth, // Note: negate z because we are using a right-handed system,
        focalLength * v.y / -v.z + halfHeight,      // where the camera points to -z.
        v.z)
  }

  /**
   * Projects the points of a bounding box onto the image space defined by u=0..width and
   * v=0..height.
   * @param b the bounding box to project
   * @return the projected bounding box
   */
  def project(b: FilledBoundingBox): FilledBoundingBox = {
    val newLow = project(b.lowBound)
    val newUp = project(b.upBound)
    BoundingBox.empty
      .expand(newLow)
      .expand(newUp)
  }

  /**
   * Determines if a certain projected bounding box is contained within the camera's
   * view frustum, at least partially.
   * The projected bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be in camera space.
   * This will also clip objects between the camera and the near plane.
   * @param boundingBox the bounding box to check
   * @return the visibility of the bounding box
   */
  def containsBoundingBox(boundingBox: BoundingBox): Boolean = {
    boundingBox match {
      case EmptyBoundingBox() => false
      case FilledBoundingBox(lowBound, upBound) =>
        upBound.x >= 0.0f &&
          lowBound.x <= width &&
          upBound.y >= 0.0f &&
          lowBound.y <= height &&
          lowBound.z < 0.0f
    }
  }

  def toCamera: Camera = new Camera(worldToCamera, fieldOfView, width, height)

}
