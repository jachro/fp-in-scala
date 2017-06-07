package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex15Spec extends WordSpec with Matchers {

  "tails" should {

    "return an empty Stream when called on an empty Stream" in {
      Stream.empty[Int].tails.toList shouldBe List(Stream.empty[Int])
    }

    "return Stream of Streams derived from the Stream the method was called on " +
      "where the first item is the whole Stream, the second is the Stream without the first item and so on" in {
      Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(
        List(1, 2, 3),
        List(2, 3),
        List(3),
        Nil
      )
    }
  }
}