package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex2Spec extends WordSpec with Matchers {

  "tail" should {
    "return all list elements except first element" in {
      val list = List(1, 2, 3)
      List.tail(list) should be (List(2, 3))
    }

    "return Nil if list is empty" in {
      List.tail(List()) should be (Nil)
    }

    "return Nil if list is Nil" in {
      List.tail(Nil) should be (Nil)
    }
  }

  "instance tail" should {

    "return all list elements except first element" in {
      List(1, 2, 3).tail should be (List(2, 3))
    }

    "return Nil if list is empty" in {
      List().tail should be (Nil)
    }

    "return Nil if list is Nil" in {
      Nil.tail should be (Nil)
    }
  }
}
