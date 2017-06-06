package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex10Spec extends WordSpec with Matchers {

  "fibs" should {

    "return an infinite Stream of Fibonacci numbers" in {
      Stream.fibs.takeWhile(_ < 20).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
    }
  }
}