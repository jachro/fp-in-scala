package chapter8

import chapter6.RNG
import org.scalatest.{Matchers, WordSpec}

class Ex4Spec extends WordSpec with Matchers {

  import Gen._
  import TestRng._

  "choose" should {

    "return a Gen for generating zeros if the given range is <0, 1)" in {

      val gen = choose(0, 1)

      val rng = RNGZ5startingFrom(0)

      val (v, nextRng) = gen.sample(rng)
      v shouldBe 0

      gen.sample(nextRng)._1 shouldBe 0
    }

    "return a Gen for generating Ints from a range <start, stopExclusive)" in {

      val gen = choose(0, 4)

      val rng: RNG = RNGZ5startingFrom(0)

      val (list, _) = (0 to 6).foldLeft(List.empty[Int] -> rng) {
        case ((l, nextRng), _) =>
          val (v, nnRng) = gen.sample(nextRng)
          (v :: l) -> nnRng
      }

      list.reverse shouldBe List(0, 1, 2, 3, 0, 1, 2)
    }
  }
}
