package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex16Spec extends WordSpec with Matchers {

  "Function increment" should {
    "transform a given list into a new list with all the items incremented by one" in {
      List.increment(List(1, 2, 3, 10)) should be (List(2, 3, 4, 11))
    }
  }
}
