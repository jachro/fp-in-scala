package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex21Spec extends WordSpec with Matchers {

  import List._

  "filterOnFlatMap" should {

    "do nothing on empty list" in {
      filterOnFlatMap[Int](Nil)(_ % 2 == 0) shouldBe Nil
    }

    "leave all elements satisfying the given predicate" in {
      filterOnFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
    }

    "do nothing when no elements satisfy the given predicate" in {
      filterOnFlatMap[Int](List(1, 2, 3, 4))(_ > 0) shouldBe List(1, 2, 3, 4)
    }
  }
}
