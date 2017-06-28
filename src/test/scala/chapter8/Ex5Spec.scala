package chapter8

import org.scalatest.{Matchers, WordSpec}

class Ex5Spec extends WordSpec with Matchers {

  import Gen._
  import TestRng._

  "unit" should {

    "return the given value" in {

      val gen = unit(0)

      val rng = RNGZ5startingFrom(10)

      val (v, nextRng) = gen.sample(rng)
      v shouldBe 0

      gen.sample(nextRng)._1 shouldBe 0
    }
  }

  "boolean" should {

    "return sample boolean values" in {

      val gen = boolean

      val rng = RNGZ5startingFrom(0)

      val (v, nextRng) = gen.sample(rng)
      v shouldBe true

      gen.sample(nextRng)._1 shouldBe false
    }
  }

  "listOfN" should {

    "return a list of length on n with the values generated using the given Gen" in {

      val gen = listOfN(3, boolean)

      implicit val rng = RNGZ5startingFrom(0)

      val r1 :: r2 :: Nil = gen.call(2)

      r1 shouldBe List(true, false, true)
      r2 shouldBe List(false, true, true)
    }
  }

  "tuple" should {

    "be doable from the existing Gen methods" in {

      val gen = tuple(nonNegativeInt)

      implicit val rng = RNGZ5startingFrom(0)

      val t1 :: t2 :: Nil = gen.call(2)

      t1 shouldBe 0 -> 1
      t2 shouldBe 2 -> 3
    }
  }
}
