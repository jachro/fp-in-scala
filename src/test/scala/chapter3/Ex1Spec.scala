package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex1Spec extends WordSpec with Matchers {

  "matchedValue" should {

    "be 3" in {
      Ex1.matchedValue should be(3)
    }
  }
}
