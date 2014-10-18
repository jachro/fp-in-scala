package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex10Spec extends WordSpec with Matchers {

  "foldLeft" should {
    "be usable to to calculate size of the given list" in {
      List.foldLeft(List(1, 2, 3), 0){(a,h) => a + 1} should be (3)
    }

    "be usable to sum up given list's items" in {
      List.foldLeft(List(1, 2, 3), 0){_ + _} should be (6)
    }
  }
}
