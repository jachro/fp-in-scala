package chapter6

import org.scalatest.{Matchers, WordSpec}

class Ex2Spec extends WordSpec with Matchers {

  "double" should {

    "return 0 if RNG.nextInt returns 0" in {

      val (v, nextRng) = RNG.double(TestRng(0))

      v shouldBe 0D
      nextRng should not be TestRng(0)
    }

    "return 0 if RNG.nextInt returns Int.MaxInt" in {

      val (v, nextRng) = RNG.double(TestRng(Int.MaxValue))

      v shouldBe 0D
      nextRng should not be TestRng(Int.MaxValue)
    }

    "return a double value using the expression: (b % Int.MaxValue).toDouble/Int.MaxValue" in {

      val (v, nextRng) = RNG.double(TestRng(3))

      v shouldBe 3.toDouble/Int.MaxValue
      nextRng should not be TestRng(3)
    }

    "use the expression as in the test above and drop minus " +
      "if the RNG.nextInt returns a negative number" in {

      val (v, nextRng) = RNG.double(TestRng(-3))

      v shouldBe 3.toDouble/Int.MaxValue
      nextRng should not be TestRng(-3)
    }

    "use the expression as in the test above and drop minus " +
      "if the RNG.nextInt returns the Int.MinValue" in {

      val (v, nextRng) = RNG.double(TestRng(Int.MinValue))

      v shouldBe 1.toDouble/Int.MaxValue
      nextRng should not be TestRng(Int.MinValue)
    }

    "return the same results when called multiple times on the same RNG object" in {

      val (v, nextRng) = RNG.double(TestRng(0))

      v shouldBe 0D
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = RNG.double(TestRng(0))

      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }

    "return different results when called on the returned RNG" in {

      val (v, nextRng) = RNG.double(TestRng(0))

      v shouldBe 0D
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = RNG.double(nextRng)

      v1 should not be v
      v1 should be > 0D
      nextRng1 should not be nextRng
    }
  }

  private case class TestRng(n: Int) extends RNG {

    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }
}