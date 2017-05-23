package chapter4

import org.scalatest.{Matchers, WordSpec}

class Ex6Spec extends WordSpec with Matchers {

  "map" should {

    "return Left if called on a Left" in {
      Left("error").map(_ => 1) shouldBe Left("error")
    }

    "return Right with converted value if called on a Right" in {
      Right(1).map(_.toString) shouldBe Right("1")
    }
  }

  "flatMap" should {

    "return Left if called on Left" in {
      Left("error").flatMap(_ => Right(1)) shouldBe Left("error")
    }

    "return Right with converted value if called on Right" in {
      Right(1).flatMap(v => Right(v.toString)) shouldBe Right("1")
    }

    "return Left if called on Right but the given function returns Left" in {
      Right(1).flatMap(_ => Left("error")) shouldBe Left("error")
    }
  }

  "orElse" should {

    "return the Right's value" in {
      Right(1).orElse(Right("1")) shouldBe Right(1)
    }

    "return the value passed to the 'orElse's call when invoked on Left" in {
      Left("error").orElse(Right("1")) shouldBe Right("1")
    }

    "return Left passed as the 'orElse' arg when called on Left" in {
      Left("error").orElse(Left("another error")) shouldBe Left("another error")
    }
  }

  "map2" should {

    val f = (i: Int, s: String) => i -> s

    "map values of the Right the call is made on " +
      "and the Right passed to the method " +
      "using the given 'f'" in {
      Right(1).map2(Right("1"))(f) shouldBe Right(1 -> "1")
    }

    "return the Left the map2 is invoked" in {
      Left("error").map2(Right("1"))(f) shouldBe Left("error")
    }

    "return the Left passed as the map2 arg when it's invoked on Right" in {
      Right(1).map2(Left("error"))(f) shouldBe Left("error")
    }

    "return the Left the map2 is invoked " +
      "even when another Left is passed as an map2 arg" in {
      Left("error1").map2(Left("error2"))(f) shouldBe Left("error1")
    }
  }
}