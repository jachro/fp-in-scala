package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex5Spec extends WordSpec with Matchers {

  "dropWhile" should {
    "remove elements as long as the given predicate is met" in {
      List.dropWhile(List(1, 2, 3, 4), (a: Int) => a < 3) should be (List(3, 4))
    }

    "removes nothing if given predicate is not met" in {
      val list = List(1, 2, 3, 4)
      List.dropWhile(list, (a: Int) => a < 0) should be (list)
    }

    "removes all if given predicate is met for all items" in {
      val list = List(1, 2, 3, 4)
      List.dropWhile(list, (a: Int) => a < 10) should be (Nil)
    }

    "does nothing if Nil passed" in {
      List.dropWhile(Nil, (a: Any) => true) should be (Nil)
    }
  }
}
