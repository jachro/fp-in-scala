package chapter4

import org.scalatest.{Matchers, WordSpec}

class Ex8Spec extends WordSpec with Matchers {

  "map2Nel" should {

    val f = (i: Int, s: String) => i -> s

    "map values of the Right the call is made on " +
      "and the Right passed to the method " +
      "using the given 'f'" in {
      Right(1).map2Nel(Right("1"))(f) shouldBe Right(1 -> "1")
    }

    "return the Left the map2Nel is invoked" in {
      Left("error").map2Nel(Right("1"))(f) shouldBe Left(List("error"))
    }

    "return the Left passed as the map2Nel arg when it's invoked on Right" in {
      Right(1).map2Nel(Left("error"))(f) shouldBe Left(List("error"))
    }

    "return both the errors " +
      "when the Left the map2Nel is invoked on and the Left passed to it" in {
      Left("error1").map2Nel(Left("error2"))(f) shouldBe Left(List("error1", "error2"))
    }
  }
}