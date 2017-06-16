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

  private case class TestRng(n: Int) extends RNG {
    def nextInt: (Int, RNG) = n -> TestRng(n + 1)
  }

}