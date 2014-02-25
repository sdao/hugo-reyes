package edu.utexas.cs.sdao.reyes.core

import org.scalatest._

/**
 * Unit tests for the Matrix4 class.
 */
class Matrix4Test extends FlatSpec with ShouldMatchers {

  "a matrix" should "correctly translate bounding spheres" in {
    val bSphere = FilledBoundingSphere(Vector3(1.0f, 2.0f, 3.0f), 2.0f)

    val translate = Matrix4.translation(Vector3(1.0f, 2.0f, 3.0f))
    val bSphereTranslated = translate * bSphere
    bSphereTranslated.origin should be (Vector3(2.0f, 4.0f, 6.0f))
    bSphereTranslated.radius should be (2.0f)
  }

  it should "correctly scale bounding spheres" in {
    val bSphere = FilledBoundingSphere(Vector3.ZERO, 2.0f)

    val scale1 = Matrix4.scaling(Vector3(1.0f, 2.0f, 3.0f))
    val bSphereScaled1 = scale1 * bSphere
    bSphereScaled1.origin should be (Vector3.ZERO)
    bSphereScaled1.radius should be (6.0f)

    val scale2 = Matrix4.scaling(Vector3(1.0f, -2.2f, 0.8f))
    val bSphereScaled2 = scale2 * bSphere
    bSphereScaled2.origin should be (Vector3.ZERO)
    bSphereScaled2.radius should be (4.4f)
  }

  it should "correctly rotate bounding spheres" in {
    val bSphere = FilledBoundingSphere(Vector3.ZERO, 2.0f)

    val rotate = Matrix4.rotationX(math.Pi.toFloat)
    val bSphereRotated = rotate * bSphere
    bSphereRotated.origin should be (Vector3.ZERO)
    bSphereRotated.radius should be (2.0f)
  }

}
