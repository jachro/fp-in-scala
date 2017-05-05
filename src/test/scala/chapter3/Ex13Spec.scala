package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex13Spec extends WordSpec with Matchers {

  "foldRight using foldLeft" should {
    "work as foldRight" in {
      List.foldRightOnFoldLeft[Int, String](List(1, 2, 3), "")((x, acc) => acc + x) shouldBe "321"
    }
  }

  "foldLeft using foldRight" should {
    "work as foldLeft" in {
      List.foldLeftOnFoldRight[Int, String](List(1, 2, 3), "")((acc, x) => acc + x) shouldBe "123"
    }
  }
}
