package chapter6

import org.scalatest.{Matchers, WordSpec}

class Ex10Spec extends WordSpec with Matchers {

  import RNG._

  private val doubleRand = RNG.mapOnFlatMap(nonNegativeInt) { int =>
    (int % Int.MaxValue).toDouble / Int.MaxValue
  }

  "State.unit" should {

    "return the given value" in {

      def const(int: Int) = State(nonNegativeInt) unit int

      val (v, nextRng) = const(1)(TestRng(0))

      v shouldBe 1
      nextRng shouldBe TestRng(0)

      val (v1, nextRng1) = const(1)(TestRng(0))
      v1 shouldBe v
      nextRng1 shouldBe nextRng
    }
  }

  "State.map" should {

    val doubleGen = State(nonNegativeInt) map { int =>
      (int % Int.MaxValue).toDouble / Int.MaxValue
    }

    "allow to build double generator" in {

      val (v, nextRng) = doubleGen(TestRng(0))

      v shouldBe 0D
      nextRng should not be TestRng(0)

      val (v1, nextRng1) = doubleGen(TestRng(Int.MaxValue))

      v1 shouldBe 0D
      nextRng1 should not be TestRng(Int.MaxValue)
    }
  }

  "State.map2" should {

    val intDoubleGen = State.map2(nonNegativeInt, doubleRand)(_ -> _)

    "allow to build (int, double) generator" in {

      val (v, nextRng) = intDoubleGen(TestRng(0))

      v shouldBe 0 -> 1.toDouble / Int.MaxValue
      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)
    }
  }

  "State.flatMap" should {

    def nonNegativeLessThan(n: Int): RNG => (Int, RNG) = State(nonNegativeInt) flatMap[Int] {
      int => {
        val mod = int % n
        if (int + (n - 1) - mod >= 0) rng => mod -> rng
        else nonNegativeLessThan(n)
      }
    }

    "return an int which is non-negative and less than the given value" in {

      val (v, rng) = nonNegativeLessThan(10)(TestRng(Int.MaxValue))

      v should (be >= 0 and be < 10)
      rng should not be TestRng(0)

    }
  }

  "State.sequence" should {

    def intsOnSequence(n: Int): State.State[RNG, List[Int]] =
      State.sequence(List.fill(n)(SimpleRNG(0).int))

    "allow to a List on ints of a given size" in {

      val (v, nextRng) = intsOnSequence(3)(TestRng(0))

      v shouldBe List(0, 1, 2)
      nextRng should not be TestRng(0)
      nextRng should not be TestRng(1)
      nextRng should not be TestRng(2)
    }
  }

  private case class TestRng(n: Int) extends RNG {
    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }

}