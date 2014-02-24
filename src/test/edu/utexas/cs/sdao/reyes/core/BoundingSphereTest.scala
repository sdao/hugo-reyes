package edu.utexas.cs.sdao.reyes.core

import org.scalatest._
import math._

/**
 * Unit tests for the BoundingSphere subclasses.
 */
class BoundingSphereTest extends FlatSpec with ShouldMatchers {

  "a bounding sphere" should "accept interior values" in {
    val vec1 = Vector3(2.0f, 2.0f, 2.0f)
    val bSphere = EmptyBoundingSphere.expand(vec1).expand(-vec1)
    bSphere.contains(vec1) should be (true)
    bSphere.contains(-vec1) should be (true)

    val vec2 = Vector3(0.0f, 0.0f, vec1.length)
    bSphere.contains(vec2) should be (true)

    val vec3 = vec2 - Vector3(0.05f, 0.05f, 0.05f)
    bSphere.contains(vec3) should be (true)
  }

  it should "reject exterior values" in {
    val vec1 = Vector3(2.0f, 2.0f, 2.0f)
    val bSphere = EmptyBoundingSphere.expand(vec1).expand(-vec1)

    val out1 = vec1 + Vector3(0.05f, 0.05f, 0.05f)
    bSphere.contains(out1) should be (false)
    bSphere.contains(-out1) should be (false)

    val vec2 = Vector3(0.0f, 0.0f, vec1.length)
    val out2 = vec2 + Vector3(0.05f, 0.05f, 0.05f)
    bSphere.contains(out2) should be (false)
  }

  it should "deal with values near the sphere boundary" in {
    val vec1 = Vector3(-1.0f, -1.0f, 0.0f)
    val vec2 = Vector3(2.0f, 3.0f, 0.0f)
    val bSphere = EmptyBoundingSphere.expand(vec1).expand(vec2)

    val r = 2.5f
    bSphere.radius should be (r)
    bSphere.origin should be (vec1 + Vector3(3.0f, 4.0f, 0.0f).normalize * r) // Slope = 4/3

    for (θ <- 0.0 to 2 * math.Pi by 0.1) {
      bSphere.contains(bSphere.origin + Vector3(cos(θ).toFloat, sin(θ).toFloat, 0.0f) * (r - 0.01f)) should be (true)
      bSphere.contains(bSphere.origin + Vector3(cos(θ).toFloat, sin(θ).toFloat, 0.0f) * (r + 0.01f)) should be (false)

      bSphere.contains(bSphere.origin + Vector3(0.0f, cos(θ).toFloat, sin(θ).toFloat) * (r - 0.01f)) should be (true)
      bSphere.contains(bSphere.origin + Vector3(0.0f, cos(θ).toFloat, sin(θ).toFloat) * (r + 0.01f)) should be (false)
    }
  }

  it should "not expand when given an interior point" in {
    val vec1 = Vector3(-1.0f, -1.0f, 0.0f)
    val vec2 = Vector3(2.0f, 3.0f, 0.0f)
    val vec3 = Vector3(2.0f, 1.0f, 0.0f)
    val bSphere1 = EmptyBoundingSphere.expand(vec1).expand(vec2)
    val bSphere2 = bSphere1.expand(vec3)

    bSphere2.radius should be (bSphere1.radius)
    bSphere2.origin should be (bSphere1.origin)
  }

  it should "combine properly with another bounding sphere" in {
    val vec1 = Vector3(1.0f, 1.0f, 1.0f)
    val bSphere1 = EmptyBoundingSphere.expand(vec1).expand(-vec1)

    val vec2 = Vector3(0.5f, 0.5f, 0.5f)
    val vec3 = Vector3(3.0f, 3.0f, 3.0f)
    val bSphere2 = EmptyBoundingSphere.expand(vec2).expand(vec3)

    val bSphere3 = bSphere1.expand(bSphere2)
    val bSphere4 = bSphere2.expand(bSphere1)
    bSphere3.radius should be (bSphere4.radius)
    bSphere3.origin should be (bSphere4.origin)
    bSphere3.radius should be ((vec3 - (-vec1)).length * 0.5f)
    bSphere3.origin should be (Vector3(1.0f, 1.0f, 1.0f))
  }

  it should "not expand when given another completely-interior sphere" in {
    val vec1 = Vector3(-1.0f, -1.0f, 0.0f)
    val vec2 = Vector3(2.0f, 3.0f, 0.0f)
    val bSphere1 = EmptyBoundingSphere.expand(vec1).expand(vec2)

    val vec3 = Vector3(-0.9f, -0.9f, 0.0f)
    val vec4 = Vector3(1.9f, 2.9f, 0.0f)
    val bSphere2 = EmptyBoundingSphere.expand(vec3).expand(vec4)

    val bSphere3 = bSphere1.expand(bSphere2)
    val bSphere4 = bSphere2.expand(bSphere1)
    bSphere3.radius should be (bSphere4.radius)
    bSphere3.origin should be (bSphere4.origin)
    bSphere3.radius should be (bSphere1.radius)
    bSphere3.origin should be (bSphere1.origin)
  }

}
