package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex14Spec extends WordSpec with Matchers {

  "startsWith" should {

    "return true if the Stream we call the method on starts with the given Stream " in {

      Stream.empty[Int].startsWith(Stream.empty[Int]) shouldBe true

      Stream(1, 2, 3).startsWith(Stream.empty[Int]) shouldBe true

      Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true

      Stream(1, 2, 3).startsWith(Stream(2)) shouldBe false

      Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) shouldBe false
    }
  }
}