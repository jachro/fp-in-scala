package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex19Spec extends WordSpec with Matchers {

  "filter" should {
    "remove element from the list until they satisfy the given predicate" in {
      List.filter(List(1, 2, 3, 4))(item => (item % 2) == 0) shouldBe List(2, 4)
    }
  }
}
