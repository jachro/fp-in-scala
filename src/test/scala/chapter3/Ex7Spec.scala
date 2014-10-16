package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex7Spec extends WordSpec with Matchers {

  "product2" should {
    "be short circuit in case of 0.0 among list elements" in {
      List.product2(List(1, 2, 0, 4)) should be (0)
    }
  }
}
