package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex15Spec extends WordSpec with Matchers {

  "concatenate" should {
    "concatenate list of lists into single list" in {
      List.concatenate(List(List(1, 2), List(3, 4, 5), List(6))) should be(List(1, 2, 3, 4, 5, 6))
    }
  }
}
