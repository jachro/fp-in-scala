package ch2dot4

import org.scalatest.{Matchers, WordSpec}

class Ex1Spec extends WordSpec with Matchers {

  "Fibonacci number finder" should {
    "return 0 for number 1" in {
      Fibonacci.findNumber(1) should be(0)
    }

    "return 1 for number 2" in {
      Fibonacci.findNumber(2) should be(1)
    }

    "return 1 for number 3" in {
      Fibonacci.findNumber(3) should be(1)
    }

    "return 2 for number 4" in {
      Fibonacci.findNumber(4) should be(2)
    }

    "return 34 for number 10" in {
      Fibonacci.findNumber(10) should be(34)
    }

    "return 144 for number 13" in {
      Fibonacci.findNumber(13) should be(144)
    }
  }
}
