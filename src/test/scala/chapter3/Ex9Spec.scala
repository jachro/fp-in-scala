package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex9Spec extends WordSpec with Matchers {

  "length based on foldRight" should {
    "calculate size of the given list" in {
      List.length(List(1, 2, 3)) should be (3)
    }
  }
}
