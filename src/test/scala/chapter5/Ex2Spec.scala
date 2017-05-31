package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex2Spec extends WordSpec with Matchers {

  "take" should {

    "return an empty Stream for take(0)" in {
      Stream(1, 2, 3).take(0) shouldBe Stream.empty[Int]
    }

    "return an empty Stream if called on an empty Stream" in {
      Stream.empty[Int].take(1) shouldBe Stream.empty[Int]
    }

    "return a Stream with the first n elements of the Stream" in {
      Stream(1, 2, 3).take(2).toList shouldBe List(1, 2)
    }
  }

  "drop" should {

    "return the same Stream for drop(0)" in {
      Stream(1, 2, 3).drop(0).toList shouldBe Stream(1, 2, 3).toList
    }

    "return an empty Stream if called on an empty Stream" in {
      Stream.empty[Int].drop(1) shouldBe Stream.empty[Int]
    }

    "return a Stream without the first n elements of the Stream" in {
      Stream(1, 2, 3).drop(2).toList shouldBe List(3)
    }
  }
}