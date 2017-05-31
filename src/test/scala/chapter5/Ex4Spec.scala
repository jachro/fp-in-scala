package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex4Spec extends WordSpec with Matchers {

  "forAll" should {

    "return true for an empty Stream" in {
      Stream.empty[Int].forAll(_ < 3) shouldBe true
    }

    "return false when there are no elements matching the predicate" in {
      Stream(1, 2, 3).forAll(_ < 1) shouldBe false
    }

    "return false if there is a non-matching element " +
      "and it's not the first one" in {
      Stream(1, 2, 3).forAll(_ == 1) shouldBe false
    }

    "return true if all elements satisfy the predicate" in {
      Stream(1, 2, 3).forAll(_ < 4) shouldBe true
    }
  }
}