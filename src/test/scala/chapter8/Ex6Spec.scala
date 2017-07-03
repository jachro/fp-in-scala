package chapter8

import org.scalatest.{Matchers, WordSpec}

class Ex6Spec extends WordSpec with Matchers {

  import Gen._
  import TestRng._

  "listOfNOnFlatMap" should {

    "return a list of length on n with the values generated using the given Gen" in {

      val gen = listOfNOnFlatMap(3, boolean)

      implicit val rng = RNGZ5startingFrom(0)

      val r1 :: r2 :: Nil = gen.call(2)

      r1 shouldBe List(true, false, true)
      r2 shouldBe List(false, true, true)
    }
  }
}
