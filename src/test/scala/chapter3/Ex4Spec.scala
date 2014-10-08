package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex4Spec extends WordSpec with Matchers {

  "drop" should {
    "remove n first elements from the list" in {
      List.drop(List(1, 2, 3, 4), 2) should be (List(3, 4))
    }

    "return Nil if passed Nil" in {
      List.drop(Nil, 2) should be (Nil)
    }
  }
}
