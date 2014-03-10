package edu.utexas.cs.sdao.reyes.anim

import math._
import edu.utexas.cs.sdao.reyes.render.Camera
import edu.utexas.cs.sdao.reyes.graph.SurfaceNode
import edu.utexas.cs.sdao.reyes.shading.ShadowMappedLight
import scala.collection.mutable
import edu.utexas.cs.sdao.reyes.core.Texture
import javax.swing.{SwingUtilities, JFrame}
import edu.utexas.cs.sdao.reyes.ui.TexturePanel
import java.awt.Dimension

/**
 * A timeline that keeps track of the elapsed time in an animation.
 *
 * Warning: timelines contain state, even though the state is not
 * directly modifiable by an outsider. The state changes as an
 * animated scene renders and the timeline advances.
 *
 * Note: the start time does not correspond to frame 0. The time
 * 0.0 seconds corresponds to frame 0; all other values will be
 * interpolated based on the frame rate.
 *
 * @param fps the frame rate of the animation
 * @param startTime the start time of the animation;
 *                  the animation will begin at the closest frame on or after this time,
 *                  which can be queried through the `firstFrameTime` value
 * @param endTime the end time of the animation;
 *                the animation will end at the closest frame on or before this time,
 *                which can be queries through the `lastFrameTime` value
 */
case class Timeline(fps: Float = 24.0f,
                    startTime: Float = 0.0f,
                    endTime: Float = 0.0f) {

  private val lock = new Object()

  /**
   * The frame number of the first frame in this animation sequence.
   */
  val firstFrame = timeToFrame(startTime)

  /**
   * The actual starting time of the animation; this is the time of the first frame.
   */
  val firstFrameTime = frameToTime(firstFrame)

  /**
   * The frame number of the last frame in this animation sequence.
   */
  val lastFrame = timeToFrame(endTime)

  /**
   * The actual ending time of the animation; this is the time of the last frame.
   */
  val lastFrameTime = frameToTime(lastFrame)

  private var secondsElapsed = 0.0f

  /**
   * Aligns a given time to the frame boundary.
   * @param secs the time to align
   * @return the aligned time
   */
  private def alignTime(secs: Float): Float = {
    round(secs * fps) / fps
  }

  /**
   * Converts a time to the closest frame number.
   * @param secs a time
   * @return the closest frame number
   */
  def timeToFrame(secs: Float): Int = {
    round(secs * fps)
  }

  /**
   * Converts a frame number to the exact corresponding time.
   * @param frame a frame number
   * @return the exact corresponding time
   */
  def frameToTime(frame: Int): Float = {
    frame.toFloat / fps
  }

  /**
   * Gets the current time state of the clock.
   * @return the current time, in seconds
   */
  def time: Float = lock.synchronized {
    secondsElapsed
  }

  /**
   * Gets the current frame state of the clock.
   * @return the current frame number
   */
  def frame: Int = lock.synchronized {
    timeToFrame(secondsElapsed)
  }

  /**
   * Sets the current time state of the clock.
   * @param secs the time, in seconds
   * @return whether the time was successfully set to the exact value given;
   *         if false, the time was limited to the first or last frame's time
   */
  private def setTime(secs: Float): Boolean = lock.synchronized {
    if (secs < firstFrameTime) {
      secondsElapsed = firstFrameTime
      false
    } else if (secs > lastFrameTime) {
      secondsElapsed = lastFrameTime
      false
    } else {
      secondsElapsed = alignTime(secs)
      true
    }
  }

  /**
   * Sets the current frame state of the clock.
   * @param frame the frame number
   * @return whether the frame was successfully set to the exact value given;
   *         if false, the frame was limited to the first or last frame
   */
  private def setFrame(frame: Int): Boolean = lock.synchronized {
    setTime(frameToTime(frame))
  }

  /**
   * Advances the current frame state by one frame.
   * @return whether the frame was successfully advanced;
   *         if false, the frame was limited to the last frame
   */
  private def goNextFrame(): Boolean = lock.synchronized {
    setFrame(frame + 1)
  }

  /**
   * Moves the current frame to the first frame.
   */
  private def goFirstFrame(): Unit = lock.synchronized {
    setFrame(firstFrame)
  }

  def renderSequence(cam: Camera,
                     sceneRoot: SurfaceNode,
                     lights: Vector[ShadowMappedLight] = Vector.empty): Sequence = {
    val allRenders = mutable.MutableList[Texture]()

    goFirstFrame()
    do {
      lights.foreach(_.renderShadowMap(sceneRoot))
      allRenders += cam.render(sceneRoot)._1
    } while (goNextFrame())

    Sequence(allRenders.toVector, this)
  }

  def renderSequenceInteractive(cam: Camera,
                                sceneRoot: SurfaceNode,
                                lights: Vector[ShadowMappedLight] = Vector.empty): Sequence = {
    val window = new JFrame("Render Output")
    val panel = new TexturePanel()

    window.setPreferredSize(new Dimension(cam.outputDimensions._1, cam.outputDimensions._2))
    window.add(panel)
    window.pack()
    window.setVisible(true)

    val allRenders = mutable.MutableList[Texture]()

    println(s"First frame: $firstFrame; Last frame: $lastFrame")
    goFirstFrame()
    do {
      lights.foreach(_.renderShadowMap(sceneRoot))
      allRenders += cam.render(sceneRoot, displaceOnly = false, t => {
        SwingUtilities.invokeAndWait(new Runnable {
          def run(): Unit = {
            panel.setImage(t)
            panel.repaint()
          }
        })
      })._1
      println(s"Rendered frame $frame")
    } while (goNextFrame())

    Sequence(allRenders.toVector, this)
  }

}
