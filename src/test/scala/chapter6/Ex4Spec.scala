package chapter6

import org.scalatest.{Matchers, WordSpec}

class Ex4Spec extends WordSpec with Matchers {

  "ints" should {

    "return an empty List if given count is 0" in {
      RNG.ints(0)(TestRng(0)) shouldBe Nil -> TestRng(0)
    }

    "return List of given number of elements " +
      "where each following element is generated with a call to the nextInt on the previous RND object" in {

      val (v, nextRng) = RNG.ints(3)(TestRng(0))

      v shouldBe List(0, 1, 2)
      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)
      nextRng should not be TestRng(2)
    }

    "return the same results when called multiple times on the same RNG object" in {

      val (v, nextRng) = RNG.ints(3)(TestRng(0))

      v should have size 3
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = RNG.ints(3)(TestRng(0))

      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }

    "return different results when called on the returned RNG" in {

      val (v, nextRng) = RNG.ints(3)(TestRng(0))

      v should have size 3
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = RNG.ints(3)(nextRng)

      v1 should not be v
      v1 should have size 3
      nextRng1 should not be nextRng
    }
  }

  private case class TestRng(n: Int) extends RNG {

    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }
}