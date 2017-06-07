package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex13Spec extends WordSpec with Matchers {

  "unfold" should {

    "allow to be used to build the 'map' method" in {

      Stream.empty[Int].mapOnUnfold(_.toString) shouldBe Stream.empty[Int]

      Stream(1, 2, 3).mapOnUnfold(_.toString).toList shouldBe List("1", "2", "3")
    }
  }
}