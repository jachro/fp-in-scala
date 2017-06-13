package chapter6

import org.scalatest.{Matchers, WordSpec}

class Ex1Spec extends WordSpec with Matchers {

  "nonNegativeInt" should {

    "return 0 if RNG.nextInt returns 0" in {

      val (v, nextRng) = RNG.nonNegativeInt(TestRng(0))

      v shouldBe 0
      nextRng should not be TestRng(0)
    }

    "return Int.MaxValue if RNG.nextInt returns Int.MaxValue" in {

      val (v, nextRng) = RNG.nonNegativeInt(TestRng(Int.MaxValue))

      v shouldBe Int.MaxValue
      nextRng should not be TestRng(Int.MaxValue)
    }

    "return 0 if RNG.nextInt returns Int.MinValue" in {

      val (v, nextRng) = RNG.nonNegativeInt(TestRng(Int.MinValue))

      v shouldBe 0
      nextRng should not be TestRng(Int.MinValue)
    }

    "return a positive Int if RNG.nextInt returns one" in {

      val (v, nextRng) = RNG.nonNegativeInt(TestRng(3))

      v shouldBe 3
      nextRng should not be TestRng(3)
    }

    "return a positive Int if RNG.nextInt returns negative number" in {

      val (v, nextRng) = RNG.nonNegativeInt(TestRng(-3))

      v shouldBe 3
      nextRng should not be TestRng(-3)
    }

    "return 0 if RNG.nextInt returns Int.MinInt" in {

      val (v, nextRng) = RNG.nonNegativeInt(TestRng(Int.MinValue))

      v shouldBe 0
      nextRng should not be TestRng(Int.MinValue)
    }

    "return the same results when called multiple times on the same RNG object" in {

      val (v, nextRng) = RNG.nonNegativeInt(TestRng(0))

      v should be >= 0
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = RNG.nonNegativeInt(TestRng(0))

      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }

    "return different results when called on the returned RNG" in {

      val (v, nextRng) = RNG.nonNegativeInt(TestRng(0))

      v should be >= 0
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = RNG.nonNegativeInt(nextRng)

      v1 should not be v
      v1 should be >= 0
      nextRng1 should not be nextRng
    }
  }

  private case class TestRng(n: Int) extends RNG {

    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }
}