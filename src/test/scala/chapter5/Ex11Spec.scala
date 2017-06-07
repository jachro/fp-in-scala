package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex11Spec extends WordSpec with Matchers {

  "unfold" should {

    "allow to build an infinite Stream of Int values starting from a given value" in {
      Stream.unfold(0) {
        previous => Some(previous, previous + 1)
      }.takeWhile(_ < 10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "allow to build a finite Stream" in {

      var counter = 0

      Stream.unfold(0) { _ =>
        if (counter < 5) {
          counter = counter + 1
          Some(counter, counter)
        } else None
      }.toList shouldBe List(1, 2, 3, 4, 5)
    }
  }
}