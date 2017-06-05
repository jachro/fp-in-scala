package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex6Spec extends WordSpec with Matchers {

  "headOption built with foldRight" should {

    "return None for an empty Stream" in {
      Stream.empty[Int].headOption shouldBe None
    }

    "return Some containing the first element of the Stream" in {
      Stream(1, 2).headOption shouldBe Some(1)
    }
  }
}