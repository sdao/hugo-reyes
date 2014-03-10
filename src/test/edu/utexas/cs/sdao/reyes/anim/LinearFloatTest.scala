package edu.utexas.cs.sdao.reyes.anim

import org.scalatest._
import edu.utexas.cs.sdao.reyes.render.Camera
import edu.utexas.cs.sdao.reyes.graph.NullNode

/**
 * Unit tests for the LinearFloat animatable parameter.
 */
class LinearFloatTest extends FlatSpec with ShouldMatchers {

  "a linear float parameter" should "interpolate in the normal range" in {
    val timeStart = Timeline(startTime = 0.0f, endTime = 0.0f)
    val timeMiddle = Timeline(startTime = 0.75f, endTime = 0.75f)
    val timeEnd = Timeline(startTime = 1.0f, endTime = 1.0f)

    val c = new Camera()
    val n = NullNode()
    val paramStart = LinearFloat(2.0f, 4.0f, 0.0f, 1.0f, timeStart)
    val paramMiddle = LinearFloat(2.0f, 4.0f, 0.0f, 1.0f, timeMiddle)
    val paramEnd = LinearFloat(2.0f, 4.0f, 0.0f, 1.0f, timeEnd)

    /* Move timelines to their endTimes. */
    timeStart.renderSequence(c, n)
    timeMiddle.renderSequence(c, n)
    timeEnd.renderSequence(c, n)

    timeStart.time should be (0.0f)
    timeMiddle.time should be (0.75f)
    timeEnd.time should be (1.0f)

    paramStart() should be (2.0f)
    paramMiddle() should be (3.5f)
    paramEnd() should be (4.0f)
  }

  it should "interpolate before the starting time" in {
    val timeBefore = Timeline(startTime = -1.0f, endTime = -1.0f)
    val c = new Camera()
    val n = NullNode()
    val paramBefore = LinearFloat(2.0f, 4.0f, 0.0f, 1.0f, timeBefore)

    timeBefore.renderSequence(c, n)
    timeBefore.time should be (-1.0f)
    paramBefore() should be (0.0f)
  }

  it should "interpolate after the ending time" in {
    val timeAfter = Timeline(startTime = 3.0f, endTime = 3.0f)
    val c = new Camera()
    val n = NullNode()
    val paramAfter = LinearFloat(2.0f, 4.0f, 0.0f, 1.0f, timeAfter)

    timeAfter.renderSequence(c, n)
    timeAfter.time should be (3.0f)
    paramAfter() should be (8.0f)
  }

}
