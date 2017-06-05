package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex9Spec extends WordSpec with Matchers {

  "int" should {

    "return an infinite Stream of Ints starting from a given number" in {
      Stream.ints(0).takeWhile(_ < 10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
  }
}