package chapter3

import org.scalatest.{WordSpec, Matchers}

class Ex14Spec extends WordSpec with Matchers {

  "append" should {
    "add an item at the end of the list" in {
      List.append(List(1, 2), 3) should be(List(1, 2, 3))
    }
  }
}
