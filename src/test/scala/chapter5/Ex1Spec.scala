package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex1Spec extends WordSpec with Matchers {

  "toList" should {

    "return Nil if called on an empty Stream" in {
      Stream.empty[Int].toList shouldBe Nil
    }

    "return a list with all Stream's items if called on non-empty Stream" in {
      Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
    }
  }
}