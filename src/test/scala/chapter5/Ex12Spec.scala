package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex12Spec extends WordSpec with Matchers {

  "unfold" should {

    "allow to build an infinite Stream of Int values starting from a given value" in {
      Stream.unfold(0) {
        previous => Some(previous, previous + 1)
      }.takeWhile(_ < 10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "allow to build an infinite Stream of Fibonacci numbers" in {
      Stream.unfold(0 -> 1) {
        case (previous, next) => Some(previous, next -> (previous + next))
      }.takeWhile(_ < 20).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
    }

    "allow to build an infinite Stream of given object" in {

      val obj = "1"

      Stream.unfold(obj) {
        previous => Some(previous, previous)
      }.take(3).toList shouldBe Nil.padTo(3, obj)
    }

    "allow to build an infinite Stream of 1" in {
      Stream.unfold(1) {
        previous => Some(previous, previous)
      }.take(3).toList shouldBe Nil.padTo(3, 1)
    }
  }
}