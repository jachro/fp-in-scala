package chapter6

import org.scalatest.{Matchers, WordSpec}

class Chapter6Spec extends WordSpec with Matchers {

  "unit" should {

    "return the same value and state each time is called" in {

      RNG.unit(1)(TestRng(0)) shouldBe 1 -> TestRng(0)

      RNG.unit(1)(TestRng(10)) shouldBe 1 -> TestRng(10)
    }
  }

  "map" should {

    "allow to build other generators base on exisiting ones" in {

      val positiveEven = RNG.map(RNG.nonNegativeInt)(i => i - i % 2)

      positiveEven(TestRng(4))._1 shouldBe 4
      positiveEven(TestRng(5))._1 shouldBe 4
    }
  }

  private case class TestRng(n: Int) extends RNG {
    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }
}