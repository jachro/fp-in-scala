package chapter6

import org.scalatest.{Matchers, WordSpec}

class Ex8Spec extends WordSpec with Matchers {

  import RNG._

  "flatMap" should {

    "allow to implement nonNegativeLessThan" in {

      def nonNegativeLessThan(n: Int): Rand[Int] = RNG.flatMap(nonNegativeInt) { int =>
        val mod = int % n
        if (int + (n-1) - mod >= 0) RNG => mod -> RNG
        else nonNegativeLessThan(n)
      }

      val (v, rng) = nonNegativeLessThan(10)(TestRng(Int.MaxValue))
      v should (be >= 0 and be < 10)
      rng should not be TestRng(0)
    }
  }

  private case class TestRng(n: Int) extends RNG {
    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }
}