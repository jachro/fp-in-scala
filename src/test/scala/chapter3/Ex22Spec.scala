package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex22Spec extends WordSpec with Matchers {

  import List._

  private val intCombine: (Int, Int) => Int = _ + _

  "merge" should {

    "do nothing when both lists are empty" in {
      merge(Nil, Nil)(intCombine) shouldBe Nil
    }

    "return the second list if the first list is empty" in {
      merge(Nil, List(1, 2))(intCombine) shouldBe List(1, 2)
    }

    "return the first list the second list is empty" in {
      merge(List(1, 2), Nil)(intCombine) shouldBe List(1, 2)
    }

    "add corresponding items from both given lists" in {
      merge(List(1, 2), List(3, 4))(intCombine) shouldBe List(4, 6)
    }

    "add corresponding items from both given lists and rewrite additional items from the first list" in {
      merge(List(1, 2, 3), List(3, 4))(intCombine) shouldBe List(4, 6, 3)
    }

    "add corresponding items from both given lists and rewrite additional items from the second list" in {
      merge(List(1, 2), List(3, 4, 5))(intCombine) shouldBe List(4, 6, 5)
    }
  }
}
