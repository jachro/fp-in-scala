package chapter8

import org.scalatest.{Matchers, WordSpec}

class Ex8Spec extends WordSpec with Matchers {

  import Gen._
  import TestRng._

  "weighted" should {

    "pull values from the given Gens with probability proportional to the given weights" in {

      val gen = weighted(choose(0, 9) -> 0.2, choose(10, 19) -> 0.8)

      implicit val rng = RNGZ20startingFrom(0)

      val (firstGen, secondGen) = gen.call(30).partition(_ < 10)

      println(s"f: $firstGen")
      println(s"s: $secondGen")

      firstGen.size shouldBe < (30 * 0.25)
      secondGen.size shouldBe > (30 * 0.75)
    }
  }
}
