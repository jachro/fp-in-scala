package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex6Spec extends WordSpec with Matchers {

  "init" should {

    "return list without last element" in {
      List.init(List(1, 2, 3)) should be (List(1, 2))
    }

    "return Nil if given list is one element only" in {
      List.init(List(1)) should be (Nil)
    }

    "return Nil if given list is Nil" in {
      List.init(Nil) should be (Nil)
    }
  }
}
