package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex20Spec extends WordSpec with Matchers {

  "flatMap" should {

    "do nothing if the given list is empty" in {
      List.flatMap(Nil)(i => List(i)) shouldBe Nil
    }

    "do nothing if the given function does nothing" in {
      List.flatMap(List(1, 2, 3))(i => List(i)) shouldBe List(1, 2, 3)
    }

    "insert the function result into the list" in {
      List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    }
    
    "insert the function result into the list - function producing List[String]" in {
      List.flatMap(List(1, 2, 3))(i => List(i.toString)) shouldBe List("1", "2", "3")
    }
  }
}
