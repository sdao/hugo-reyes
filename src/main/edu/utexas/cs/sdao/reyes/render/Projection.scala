package edu.utexas.cs.sdao.reyes.render

import scala.math._
import edu.utexas.cs.sdao.reyes.core._
import edu.utexas.cs.sdao.reyes.core.FilledBoundingBox
import edu.utexas.cs.sdao.reyes.core.EmptyBoundingBox

/**
 * Projects world coordinates into camera and screen coordinates.
 * Does not contain any apparatus for rendering images.
 * 
 * Points in the 3D scene can exist in one of three coordinate systems:
 * world space, camera space, and screen space.
 * World space and camera space are both in 3D coordinates; camera space
 * is just a transformation of the world space to the camera's point of view.
 * Screen space is in 2D coordinates, corresponding to the image plane;
 * the third coordinate is commonly used to represent the z-depth from the camera.
 *
 * @param cameraTransform a transformation matrix that represents how the camera
 *                        has been transformed in the world space
 * @param fieldOfView the camera's horizontal field of view in radians,
 *                    measured as the angle from the left of the visible screen to the right;
 *                    acceptable values are 0 < fieldOfView < Pi
 * @param width the width of the output image plane
 * @param height the height of the output image plane
 */
abstract class Projection(cameraTransform: Matrix4 = Matrix4.IDENTITY,
                          fieldOfView: Float = toRadians(60.0).toFloat,
                          width: Int = 800,
                          height: Int = 600) {

  if (fieldOfView <= 0.0 || fieldOfView >= Pi)
    throw  new IllegalArgumentException("fieldOfView out of range")

  /**
   * The transformation matrix that converts world to camera coordinates.
   * This is the same as the inverse of the cameraTransform parameter.
   */
  val worldToCamera = cameraTransform.invert

  /**
   * The length from the camera pinhole to the image plane.
   */
  val focalLength = (width / 2.0f) / tan(fieldOfView / 2.0f).toFloat // Trigonometry, what's that?!

  private val halfWidth = width / 2.0f
  private val halfHeight = height / 2.0f

  /**
   * Transforms a point from world space into camera space.
   * However, the function does not then transform the point into the 2D screen space;
   * the resultant coordinate is still in a 3D space.
   *
   * To also project the point into screen space, use projectToScreen instead.
   *
   * @param u the world space point to transform
   * @return the transformed vector in camera space
   */
  def transformToCamera(u: Vector3) = worldToCamera * u

  /**
   * Like transformToCamera, but normalizes the resulting vector.
   * This is useful for obtaining the vector from the focal point to the
   * given point.
   * @param u the world space point to transform
   * @return the transformed and normalized vector in camera space
   */
  def transformToCameraNorm(u: Vector3) = transformToCamera(u).normalize

  /**
   * Transforms a point from camera space back into world space.
   * @param u the camera space point to transform
   * @return the transformed vector in world space
   */
  def transformToWorld(u: Vector3) = cameraTransform * u

  /**
   * Transforms a point from world space into camera space,
   * and then projects it into the screen space defined by
   * u=0..width and v=0..height, with the origin at the lower-left corner of the screen.
   * The z-component is the z-depth of the point.
   *
   * To transform without projecting, use transformToCamera instead.
   *
   * @param u the vector to project
   * @return the projected vector
   */
  def projectToScreen(u: Vector3): Vector3 = {
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
   * Determines if a certain screen-space bounding box is contained within the camera's
   * view frustum, at least partially.
   * The bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be the z-depth of the box.
   * This will also cull objects behind the camera (positive z-depth).
   * @param boundingBox the bounding box to check
   * @return the visibility of the bounding box
   */
  def containsScreenBoundingBox(boundingBox: BoundingBox): Boolean = {
    boundingBox match {
      case EmptyBoundingBox => false
      case FilledBoundingBox(lowBound, upBound) =>
        upBound.x >= 0.0f &&
          lowBound.x <= width &&
          upBound.y >= 0.0f &&
          lowBound.y <= height &&
          lowBound.z < 0.0f
    }
  }

  /**
   * Creates a camera from this projection.
   * @return the new camera
   */
  def toCamera: Camera = new Camera(cameraTransform, fieldOfView, width, height)

}
