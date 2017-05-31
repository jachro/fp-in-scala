package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex3Spec extends WordSpec with Matchers {

  "takeWhile" should {

    "return an empty Stream if takeWhile evaluates to false for the first element" in {
      Stream(1, 2, 3).takeWhile(_ < 1) shouldBe Stream.empty[Int]
    }

    "return an empty Stream if called on an empty Stream" in {
      Stream.empty[Int].takeWhile(_ < 3) shouldBe Stream.empty[Int]
    }

    "return a Stream with only the first n elements of the Stream satisfying the predicate " +
      "even if there are elements farther in the stream for which the predicate returns true" in {
      Stream(1, 2, 3, 1).takeWhile(_ < 3).toList shouldBe List(1, 2)
    }
  }
}