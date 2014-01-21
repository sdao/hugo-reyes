package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.core._
import scala.math._
import edu.utexas.cs.sdao.reyes.geom.Surface
import edu.utexas.cs.sdao.reyes.core.EmptyBoundingBox

/**
 * A camera that supersamples when rendering the final image.
 * Up to the split stage, the camera acts like a normal camera.
 * After the split stage, the number of pixels sampled is augmented by the supersampling factor.
 *
 * @param worldToCamera a transformation matrix that converts world coordinates
 *                      to camera coordinates; e.g., if the camera is translated one unit
 *                      to the left, then this matrix will move world objects one unit to
 *                      the right
 * @param fieldOfView the camera's horizontal field of view in radians,
 *                    measured as the angle from the left of the visible screen to the right;
 *                    acceptable values are 0 < fieldOfView < Pi
 * @param logicalWidth the width of the output image plane
 * @param logicalHeight the height of the output image plane
 * @param supersample the supersampling factor, in each direction
 */
class SupersamplingCamera(worldToCamera: Matrix4 = Matrix4.IDENTITY,
                          fieldOfView: Float = toRadians(60.0).toFloat,
                          logicalWidth: Int = 800,
                          logicalHeight: Int = 600,
                          supersample: Int = 2)
  extends Camera(worldToCamera, fieldOfView,
    logicalWidth * supersample, logicalHeight * supersample) {

  /**
   * The proxy camera that is used to split objects at normal scale.
   */
  private val normalCam = new Camera(worldToCamera, fieldOfView, logicalWidth, logicalHeight)

  /**
   * Estimates whether the z-buffer occludes a projected bounding box.
   * The projected bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be in scene world space.
   *
   * Note: the supersampling camera will multiply the bounding box x- and y-coordinates
   * (but not the z-depth) by the supersampling factor.
   *
   * @param boundingBox the bounding box to check
   * @return whether the bounding box is occluded
   */
  override def estimateZBufferOcclusion(boundingBox: BoundingBox): Boolean = {
    boundingBox match {
      case EmptyBoundingBox => true
      case FilledBoundingBox(lowBound, upBound) =>
        super.estimateZBufferOcclusion(FilledBoundingBox(
          Vector3(lowBound.x * supersample, lowBound.y * supersample, lowBound.z),
          Vector3(upBound.x * supersample, upBound.y * supersample, upBound.z)
        ))
    }
  }

  /**
   * Splits primitive surfaces into smaller subsurfaces until they clear
   * the size threshold when projected.
   *
   * The result is that objects that are closer to the camera will be
   * split into more partitions.
   *
   * Note: the splitting stage is performed on the normal-scale
   * proxy camera.
   *
   * @param surfaces the surfaces to split
   * @return a list containing the split surfaces, prepared for the dicing step
   *         and sorted by increasing z-depth
   */
  override def split(surfaces: Iterable[Surface]): Vector[PipelineInfo] = normalCam.split(surfaces)

  /**
   * Returns a copy of the image buffer.
   * If this function is overridden by a subclass, then imageDimensions must
   * also be overridden.
   * @return a copy of the image buffer
   */
  override def image: Texture = buffer.scale(logicalWidth, logicalHeight)

  /**
   * The dimensions, in pixel width and height, of the image returned by
   * the function image.
   * @return
   */
  override def imageDimensions: (Int, Int) = (logicalWidth, logicalHeight)

  /**
   * Creates a camera from this projection.
   * @return the new camera
   */
  override def toCamera: Camera =
    new SupersamplingCamera(worldToCamera, fieldOfView, logicalWidth, logicalHeight, supersample)

}
