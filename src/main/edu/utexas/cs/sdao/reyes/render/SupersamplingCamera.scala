package edu.utexas.cs.sdao.reyes.render

import edu.utexas.cs.sdao.reyes.core._
import scala.math._
import edu.utexas.cs.sdao.reyes.geom.Surface
import edu.utexas.cs.sdao.reyes.core.EmptyBoundingBox
import edu.utexas.cs.sdao.reyes.anim.Sequence
import edu.utexas.cs.sdao.reyes.graph.SurfaceNode

/**
 * A camera that supersamples when rendering the final image.
 * Up to the split stage, the camera acts like a normal camera.
 * After the split stage, the number of pixels sampled is augmented by the supersampling factor.
 *
 * @param cameraTransform a transformation matrix that represents how the camera
 *                        has been transformed in the world space
 * @param fieldOfView the camera's horizontal field of view in radians,
 *                    measured as the angle from the left of the visible screen to the right;
 *                    acceptable values are 0 < fieldOfView < Pi
 * @param logicalWidth the width of the output image plane
 * @param logicalHeight the height of the output image plane
 * @param supersample the supersampling factor, in each direction
 */
class SupersamplingCamera(cameraTransform: Matrix4 = Matrix4.IDENTITY,
                          fieldOfView: Float = toRadians(60.0).toFloat,
                          logicalWidth: Int = 800,
                          logicalHeight: Int = 600,
                          supersample: Int = 2)
  extends Camera(cameraTransform, fieldOfView,
    logicalWidth * supersample, logicalHeight * supersample) {

  /**
   * The proxy camera that is used to split objects at normal scale.
   */
  private val normalCam = new Camera(cameraTransform, fieldOfView, logicalWidth, logicalHeight)

  /**
   * Estimates whether the z-buffer occludes a projected bounding box.
   * The projected bounding box's x- and y-components should be in screen space,
   * whereas the z-component should be in scene world space.
   *
   * Note: the supersampling camera will multiply the bounding box x- and y-coordinates
   * (but not the z-depth) by the supersampling factor.
   *
   * @param boundingBox the bounding box to check
   * @param zBuffer the z-buffer to check
   * @return whether the bounding box is occluded
   */
  override def estimateZBufferOcclusion(boundingBox: BoundingBox, zBuffer: ZBuffer): Boolean = {
    boundingBox match {
      case EmptyBoundingBox => true
      case FilledBoundingBox(lowBound, upBound) =>
        super.estimateZBufferOcclusion(FilledBoundingBox(
          Vector3(lowBound.x * supersample, lowBound.y * supersample, lowBound.z),
          Vector3(upBound.x * supersample, upBound.y * supersample, upBound.z)
        ), zBuffer)
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
   * The dimensions, in pixel width and height, of the image returned by
   * the rendering functions. If the images returned by the rendering function
   * are larger; they will be downscaled.
   * By default, this is simply the camera's width and height. Subclasses can
   * override this function to downsample rendered images.
   * @return the preferred image dimensions
   */
  override def outputDimensions: (Int, Int) = (logicalWidth, logicalHeight)

  /**
   * Creates a camera from this projection.
   * @return the new camera
   */
  override def toCamera: Camera =
    new SupersamplingCamera(cameraTransform, fieldOfView, logicalWidth, logicalHeight, supersample)

}
