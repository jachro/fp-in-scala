package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex23Spec extends WordSpec with Matchers {

  import List._

  private val combine: (Option[Int], Option[String]) => String = {
    case (None, None) => ""
    case (Some(i), None) => s"$i"
    case (None, Some(s)) => s"$s"
    case (Some(i), Some(s)) => s"$i - $s"
  }

  "zipWith" should {

    "do nothing when both lists are empty" in {
      zipWith(Nil, Nil)(combine) shouldBe Nil
    }

    "return the second list if the first list is empty" in {
      zipWith(Nil, List("one", "two"))(combine) shouldBe List("one", "two")
    }

    "return the first list the second list is empty" in {
      zipWith(List(1, 2), Nil)(combine) shouldBe List("1", "2")
    }

    "add corresponding items from both given lists" in {
      zipWith(List(1, 2), List("one", "two"))(combine) shouldBe List("1 - one", "2 - two")
    }

    "add corresponding items from both given lists and rewrite additional items from the first list" in {
      zipWith(List(1, 2, 3), List("one", "two"))(combine) shouldBe List("1 - one", "2 - two", "3")
    }

    "add corresponding items from both given lists and rewrite additional items from the second list" in {
      zipWith(List(1, 2), List("one", "two", "three"))(combine) shouldBe List("1 - one", "2 - two", "three")
    }
  }
}
