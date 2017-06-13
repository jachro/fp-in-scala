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

  "(Double, Int)" should {

    "return (RNG.double, 0) and if RNG.nextInt returns 0" in {

      val (v, nextRng) = RNG.doubleInt(TestRng(-1))

      v shouldBe 0D -> -1
      nextRng should not be TestRng(-1)
      nextRng should not be TestRng(0)
    }

    "return (0D, RNG.nextInt) if RNG.double returns 0" in {

      val (v, nextRng) = RNG.doubleInt(TestRng(Int.MaxValue - 1))

      v shouldBe 0D -> (Int.MaxValue - 1)
      nextRng should not be TestRng(Int.MaxValue - 1)
      nextRng should not be TestRng(Int.MaxValue)
    }

    "return (RNG.double, RNG.nextInt)" in {

      val (v, nextRng) = RNG.doubleInt(TestRng(33))

      v shouldBe 34D/Int.MaxValue -> 33
      nextRng should not be TestRng(33)
      nextRng should not be TestRng(34)
    }

    "return the same results when called multiple times on the same RNG object" in {

      val (v, nextRng) = RNG.doubleInt(TestRng(0))

      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)

      val (v1, nextRng1) = RNG.doubleInt(TestRng(0))

      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }

    "return different results when called on the returned RNG" in {

      val (v, nextRng) = RNG.doubleInt(TestRng(0))

      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)

      val (v1, nextRng1) = RNG.doubleInt(nextRng)

      v1 should not be v
      nextRng1 should not be nextRng
    }
  }

  "(Double, Double, Double)" should {

    "return (Double, Double, Double)" in {

      val (v, nextRng) = RNG.double3(TestRng(-1))

      v shouldBe (1D/Int.MaxValue, 0D, 1D/Int.MaxValue)
      nextRng should not be TestRng(-1)
      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)
    }

    "return the same results when called multiple times on the same RNG object" in {

      val (v, nextRng) = RNG.double3(TestRng(0))

      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)
      nextRng should not be TestRng(2)

      val (v1, nextRng1) = RNG.double3(TestRng(0))

      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }

    "return different results when called on the returned RNG" in {

      val (v, nextRng) = RNG.double3(TestRng(0))

      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)
      nextRng should not be TestRng(2)

      val (v1, nextRng1) = RNG.double3(nextRng)

      v1 should not be v
      nextRng1 should not be nextRng
    }
  }

  private case class TestRng(n: Int) extends RNG {

    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }
}