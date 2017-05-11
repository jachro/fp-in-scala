package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex24Spec extends WordSpec with Matchers {

  import List._

  "hasSubsequence" should {

    "return true if both the list and the subsequence are empty" in {
      hasSubsequence(Nil, Nil) shouldBe true
    }

    "return true if the given list is not empty but the subsequence is empty" in {
      hasSubsequence(List(1, 2), Nil) shouldBe true
    }

    "return true if the given list contains the subsequence comprised of single item" in {
      hasSubsequence(List(1, 2, 3), List(2)) shouldBe true
    }

    "return true if the given list contains the subsequence comprised of more than one item" in {
      hasSubsequence(List(1, 2, 3), List(2, 3)) shouldBe true
    }

    "return true if the given list and the subsequence are the same" in {
      hasSubsequence(List(1, 2, 3), List(1, 2, 3)) shouldBe true
    }

    "return false if the given list does not contain the single element subsequence" in {
      hasSubsequence(List(1, 2, 3), List(4)) shouldBe false
    }

    "return false if the given subsequence contains the list's elements but in different order" in {
      hasSubsequence(List(1, 2, 3), List(3, 2)) shouldBe false
    }

    "return false if the given subsequence contains not just the list's elements" in {
      hasSubsequence(List(1, 2, 3), List(3, 4)) shouldBe false
    }
  }
}
