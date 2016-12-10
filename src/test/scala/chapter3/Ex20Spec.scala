package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex20Spec extends WordSpec with Matchers {

  "flatMap" should {
    "insert the function result into the list" in {
      List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    }
  }
}
