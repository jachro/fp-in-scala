package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex13Spec extends WordSpec with Matchers {

  "unfold" should {

    "allow to be used to build the 'map' method" in {

      Stream.empty[Int].mapOnUnfold(_.toString) shouldBe Stream.empty[Int]

      Stream(1, 2, 3).mapOnUnfold(_.toString).toList shouldBe List("1", "2", "3")
    }

    "allow to be used to build the 'take' method" in {

      Stream.empty[Int].takeOnUnfold(2) shouldBe Stream.empty[Int]

      Stream(1, 2, 3).takeOnUnfold(2).toList shouldBe List(1, 2)
    }

    "allow to be used to build the 'takeWhile' method" in {

      Stream.empty[Int].takeWhileOnUnfold(_ < 3) shouldBe Stream.empty[Int]

      Stream(1, 2, 3).takeWhileOnUnfold(_ < 3).toList shouldBe List(1, 2)
    }
  }
}