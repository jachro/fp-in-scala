package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex11Spec extends WordSpec with Matchers {

  "length based on foldLeft" should {
    "calculates size of the given list" in {
      List.lengthOnFoldLeft(List(1, 2, 3)) should be(3)
    }
  }

  "sum based on foldLeft" should {
    "sum up given list's items" in {
      List.sumOnFoldLeft(List(1, 2, 3)) should be (6)
    }
  }

  "product based on foldLeft" should {
    "multiply given list's items" in {
      List.productOnFoldLeft(List(1, 2, 3, 4)) should be (24)
    }
  }
}
