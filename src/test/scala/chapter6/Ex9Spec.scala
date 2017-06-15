package chapter6

import org.scalatest.{Matchers, WordSpec}

class Ex9Spec extends WordSpec with Matchers {

  import RNG._

  private val doubleRand = RNG.mapOnFlatMap(nonNegativeInt) { int =>
    (int % Int.MaxValue).toDouble / Int.MaxValue
  }

  "doubleRand on mapOnFlatMap" should {

    "return 0 if RNG.nextInt returns 0" in {

      val (v, nextRng) = doubleRand(TestRng(0))

      v shouldBe 0D
      nextRng should not be TestRng(0)
    }

    "return 0 if RNG.nextInt returns Int.MaxValue" in {

      val (v, nextRng) = doubleRand(TestRng(Int.MaxValue))

      v shouldBe 0D
      nextRng should not be TestRng(Int.MaxValue)
    }

    "return a double value using the expression: (b % Int.MaxValue).toDouble/Int.MaxValue" in {

      val (v, nextRng) = doubleRand(TestRng(3))

      v shouldBe 3.toDouble / Int.MaxValue
      nextRng should not be TestRng(3)
    }

    "use the expression as in the test above and drop minus " +
      "if the RNG.nextInt returns a negative number" in {

      val (v, nextRng) = doubleRand(TestRng(-3))

      v shouldBe 3.toDouble / Int.MaxValue
      nextRng should not be TestRng(-3)
    }

    "return 0 if the nonNegative returns 0" in {

      val (v, nextRng) = doubleRand(TestRng(Int.MinValue))

      v shouldBe 0D
      nextRng should not be TestRng(Int.MinValue)
    }

    "return the same results when called multiple times on the same RNG object" in {

      val (v, nextRng) = doubleRand(TestRng(0))

      v shouldBe 0D
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = doubleRand(TestRng(0))

      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }

    "return different results when called on the returned RNG" in {

      val (v, nextRng) = doubleRand(TestRng(0))

      v shouldBe 0D
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = doubleRand(nextRng)

      v1 should not be v
      v1 should be > 0D
      nextRng1 should not be nextRng
    }
  }

  private val intDouble = RNG.map2OnFlatMap(nonNegativeInt, double)(_ -> _)

  "intDouble on map2OnFlatMap" should {

    "return (0, RNG.double) and if RNG.nextInt returns 0" in {

      val (v, nextRng) = intDouble(TestRng(0))

      v shouldBe 0 -> 1.toDouble / Int.MaxValue
      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)
    }

    "return (RNG.nextInt, 0D) if RNG.double returns 0" in {

      val (v, nextRng) = intDouble(TestRng(Int.MaxValue - 1))

      v shouldBe Int.MaxValue - 1 -> 0D
      nextRng should not be TestRng(Int.MaxValue - 1)
      nextRng should not be TestRng(Int.MaxValue)
    }

    "return (RNG.nextInt, RNG.double)" in {

      val (v, nextRng) = intDouble(TestRng(33))

      v shouldBe 33 -> 34D / Int.MaxValue
      nextRng should not be TestRng(33)
      nextRng should not be TestRng(34)
    }

    "return the same results when called multiple times on the same RNG object" in {

      val (v, nextRng) = intDouble(TestRng(0))

      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)

      val (v1, nextRng1) = intDouble(TestRng(0))

      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }

    "return different results when called on the returned RNG" in {

      val (v, nextRng) = intDouble(TestRng(0))

      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)

      val (v1, nextRng1) = intDouble(nextRng)

      v1 should not be v
      nextRng1 should not be nextRng
    }
  }

  private case class TestRng(n: Int) extends RNG {
    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }

}