package chapter3

import org.scalatest.{WordSpec, Matchers}

class Ex3Spec extends WordSpec with Matchers {

  "setHead" should {
    "replace first item in the list with the given one" in {
      List.setHead[Int](0, List(1, 2, 3)) should be (List(0, 2, 3))
    }

    "set given item as a first one if given list is Nil" in {
      List.setHead[Int](0, Nil) should be (List(0))
    }
  }
}
