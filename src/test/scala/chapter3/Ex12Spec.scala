package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex12Spec extends WordSpec with Matchers {

  "reverse using foldLeft" should {
    "return a list with items in reverse order" in {
      List.reverse(List(1, 2, 3)) should be (List(3, 2, 1))
    }
  }
}
