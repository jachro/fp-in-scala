package chapter8

import org.scalatest.{Matchers, WordSpec}

class Ex7Spec extends WordSpec with Matchers {

  import Gen._
  import TestRng._

  "union" should {

    "pull values from the given Gens with equal likelihood" in {

      val gen = union(nonNegativeInt, nonNegativeInt)

      implicit val rng = RNGstartingFrom(0)

      gen.call(30) shouldBe Stream.from(start = 1, step = 2).take(30).toList
    }
  }
}
