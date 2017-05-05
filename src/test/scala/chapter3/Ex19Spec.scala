package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex19Spec extends WordSpec with Matchers {

  "filter" should {

    "do nothing on empty list" in {
      List.filter[Int](Nil)(_ % 2 == 0) shouldBe Nil
    }

    "leave all elements satisfying the given predicate" in {
      List.filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
    }

    "do nothing when no elements satisfy the given predicate" in {
      List.filter[Int](List(1, 2, 3, 4))(_ > 0) shouldBe List(1, 2, 3, 4)
    }

  }
}
