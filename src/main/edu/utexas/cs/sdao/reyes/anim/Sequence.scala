package edu.utexas.cs.sdao.reyes.anim

import edu.utexas.cs.sdao.reyes.core.Texture


/**
 * Represents a sequence of rendered textures in an animation.
 * @param frames the vector of frames in the sequence
 * @param timeline the timeline associated with the sequence
 */
case class Sequence(frames: Vector[Texture],
                    timeline: Timeline) {

  val lastFrameAvailable = timeline.firstFrame + frames.length

  private def frameToIndex(frame: Int): Int = frame - timeline.firstFrame
  private def indexToFrame(idx: Int): Int = idx + timeline.firstFrame

  /**
   * Gets the specified frame, if it exists.
   * Otherwise, throws an `IndexOutOfBoundException`.
   * @param frame the number of the frame to get
   * @return the frame requested
   */
  def apply(frame: Int): Texture = {
    if (frame < 0 || frame > lastFrameAvailable)
      throw new IndexOutOfBoundsException(s"frame $frame was out of bounds; expected range was 0 to $lastFrameAvailable")
    else
      frames(frameToIndex(frame))
  }

  /**
   * Gets the frame closest to the specified time, if it exists.
   * Otherwise, throws an `IndexOutOfBoundException`.
   * @param secs the time to get
   * @return the frame requested
   */
  def apply(secs: Float): Texture = apply(timeline.timeToFrame(secs))

  /**
   * Gets the specified frame, if it exists.
   * Otherwise, returns `None`.
   * @param frame the number of the frame to get
   * @return the frame requested, or `None`
   */
  def get(frame: Int): Option[Texture] = {
    if (frame < 0 || frame > lastFrameAvailable)
      None
    else
      Some(frames(frameToIndex(frame)))
  }

  /**
   * Gets the frame closest to the specified time, if it exists.
   * Otherwise, returns `None`.
   * @param secs the time to get
   * @return the frame requested, or `None`
   */
  def get(secs: Float): Option[Texture] = get(timeline.timeToFrame(secs))

  /**
   * Gets the first frame in the sequence.
   * Useful for single-sequence frames.
   * @return
   */
  def firstFrame: Texture = frames(0)

  /**
   * Gets the number of rendered frames in the sequence.
   * This may differ from the length of the timeline, depending on
   * how many frames are actually rendered.
   * @return the number of frames in the sequence
   */
  def length: Int = frames.length

  /**
   * Gets an indexed-sequence representation of the sequence.
   * @return an indexed sequence of tuples; the first item is the frame number
   *         and the second item is the actual frame
   */
  def toIndexedSeq: Vector[(Int, Texture)] = {
    (0 until frames.length).map(idx => (indexToFrame(idx), frames(idx))).toVector
  }

  /**
   * Writes the first frame in the sequence to a PNG file.
   * @param name the name of the image file, preferably ending in ".png"
   * @return whether the write succeeded
   */
  def writeFirstFrameFile(name: String): Boolean = firstFrame.writeToFile(name)

  /**
   * Writes the entire rendered sequence to a series of files.
   * @param name the name of the image files, preferably ending in ".png";
   *             the name must contain the placeholder "%d" for the frame number
   */
  def writeSequenceFiles(name: String): Unit = {
    if (name.format(1) == name.format(2)) {
      throw new IllegalArgumentException("name must contain a format parameter")
    }

    toIndexedSeq.foreach(x => x._2.writeToFile(name.format(x._1)))
  }

}
