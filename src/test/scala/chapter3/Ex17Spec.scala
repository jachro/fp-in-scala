package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex17Spec extends WordSpec with Matchers {

  "mapDoublesToStrings" should {
    "convert list of doubles into list of strings" in {
      List.mapDoublesToStrings(List(1d, 2d, 3d)) shouldBe List("1.0", "2.0", "3.0")
    }
  }
}
