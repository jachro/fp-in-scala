package chapter8

import org.scalatest.{Matchers, WordSpec}

class Ex3Spec extends WordSpec with Matchers {

  "&&" should {

    "return sum of success count of both sides" in {

      val prop1 = new Prop {
        override def check = Right(2)
      }

      val prop2 = new Prop {
        override def check = Right(3)
      }

      (prop1 && prop2).check shouldBe Right(5)
    }

    "return result of first operand " +
      "if it fails" in {

      val prop1 = new Prop {
        override def check = Left("failure" -> 2)
      }

      val prop2 = new Prop {
        override def check = Right(3)
      }

      (prop1 && prop2).check shouldBe Left("failure" -> 2)
    }

    "return left of failure and sum of successes " +
      "if the second operand fails" in {

      val prop1 = new Prop {
        override def check = Right(2)
      }

      val prop2 = new Prop {
        override def check = Left("failure" -> 3)
      }

      (prop1 && prop2).check shouldBe Left("failure" -> 5)
    }
  }
}
