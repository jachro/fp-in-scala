package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex8Spec extends WordSpec with Matchers {

  "constant" should {

    "return an infinite Stream of given objects" in {
      Stream.constant("a").take(3).toList shouldBe List("a", "a", "a")
    }
  }
}