package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex6Spec extends WordSpec with Matchers {

  "init" should {
    "return list without last element" in {
      List.init[Int](List(1, 2, 3)) should be (List(1, 2))
    }

    "return Nil if given list is Nil" in {
      List.init[Int](Nil) should be (Nil)
    }
  }
}
