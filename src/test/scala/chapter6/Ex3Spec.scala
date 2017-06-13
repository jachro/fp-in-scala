package chapter6

import org.scalatest.{Matchers, WordSpec}

class Ex3Spec extends WordSpec with Matchers {

  "(Int, Double)" should {

    "return (0, RNG.double) and if RNG.nextInt returns 0" in {

      val (v, nextRng) = RNG.intDouble(TestRng(0))

      v shouldBe 0D -> 1.toDouble/Int.MaxValue
      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)
    }

    "return (RNG.nextInt, 0D) if RNG.double returns 0" in {

      val (v, nextRng) = RNG.intDouble(TestRng(Int.MaxValue - 1))

      v shouldBe Int.MaxValue - 1 -> 0D
      nextRng should not be TestRng(Int.MaxValue - 1)
      nextRng should not be TestRng(Int.MaxValue)
    }

    "return (RNG.nextInt, RNG.double)" in {

      val (v, nextRng) = RNG.intDouble(TestRng(33))

      v shouldBe 33 -> 34D/Int.MaxValue
      nextRng should not be TestRng(33)
      nextRng should not be TestRng(34)
    }

    "return the same results when called multiple times on the same RNG object" in {

      val (v, nextRng) = RNG.intDouble(TestRng(0))

      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)

      val (v1, nextRng1) = RNG.intDouble(TestRng(0))

      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }

    "return different results when called on the returned RNG" in {

      val (v, nextRng) = RNG.intDouble(TestRng(0))

      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)

      val (v1, nextRng1) = RNG.intDouble(nextRng)

      v1 should not be v
      nextRng1 should not be nextRng
    }
  }

  private case class TestRng(n: Int) extends RNG {

    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }
}