package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex18Spec extends WordSpec with Matchers {

  "map" should {
    "transform each element of the given List using the given function" in {
      List.map[Int, String](List(1, 2), item => (item + 1).toString) shouldBe List("2.0", "3.0")
    }
  }
}
