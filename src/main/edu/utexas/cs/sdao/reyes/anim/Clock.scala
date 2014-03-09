package edu.utexas.cs.sdao.reyes.anim

/**
 * A clock that keeps track of the elapsed time in an animation.
 * Warning: clocks are mutable. They keep track of state during an
 * animation sequence.
 */
case class Clock(fps: Float = 24.0f) {

  private val lock = new Object()

  val secondsPerFrame = 1.0f / fps

  var secondsElapsed = 0.0f

  def time: Float = lock.synchronized {
    secondsElapsed
  }

  def setTime(secs: Float): Unit = lock.synchronized {
    secondsElapsed = secs
  }

  def nextFrame(): Unit = lock.synchronized {
    secondsElapsed += secondsPerFrame
  }

}
