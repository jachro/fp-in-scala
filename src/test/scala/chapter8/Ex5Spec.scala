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
}
