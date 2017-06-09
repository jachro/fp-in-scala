package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex16Spec extends WordSpec with Matchers {

  "scanRight" should {

    "return an empty Stream when called on an empty Stream" in {
      Stream.empty[Int].scanRight(0)(_ + _).toList shouldBe List(0)
    }

    "return a Stream of intermediate results " +
      "of applying the function on the sequence of Streams " +
      "where the first Stream is the original one, the second Stream is the original Stream without the first item and so on" in {

      Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(1 + 2 + 3 + 0, 2 + 3 + 0, 3 + 0, 0)

      Stream(1, 2, 3).scanRight("0") {
        case (i, res) => i + res
      }.toList shouldBe List("1230", "230", "30", "0")
    }
  }

  "scanRightOnUnfoldAndFoldRight - does not reuse intermediate results" should {

    "return an empty Stream when called on an empty Stream" in {
      Stream.empty[Int].scanRightOnUnfoldAndFoldRight(0)(_ + _).toList shouldBe List(0)
    }

    "return a Stream of intermediate results " +
      "of applying the function on the sequence of Streams " +
      "where the first Stream is the original one, the second Stream is the original Stream without the first item and so on" in {

      Stream(1, 2, 3).scanRightOnUnfoldAndFoldRight(0)(_ + _).toList shouldBe List(1 + 2 + 3 + 0, 2 + 3 + 0, 3 + 0, 0)

      Stream(1, 2, 3).scanRightOnUnfoldAndFoldRight("0") {
        case (i, res) => i + res
      }.toList shouldBe List("1230", "230", "30", "0")
    }
  }
}