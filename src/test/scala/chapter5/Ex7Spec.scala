package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex7Spec extends WordSpec with Matchers {

  "map" should {

    "return an empty Stream for an empty Stream" in {
      Stream.empty[Int].map(_.toString) shouldBe Stream.empty[Int]
    }

    "return a Stream with all elements converted with the given function" in {
      Stream(1, 2).map(_.toString).toList shouldBe List("1", "2")
    }
  }

  "filter" should {

    "return an empty Stream for an empty Stream" in {
      Stream.empty[Int].filter(_ > 0) shouldBe Stream.empty[Int]
    }

    "return a Stream with all elements converted with the given function" in {
      Stream(1, 2, 3).filter(_ < 3).toList shouldBe List(1, 2)
    }
  }
}