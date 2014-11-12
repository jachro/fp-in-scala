package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex13Spec extends WordSpec with Matchers {

  "foldRight using foldLeft" should {
    "work as foldRight" in {
      List.foldRightOnFoldLeft[Int, List[Int]](List(1, 2, 3), Nil)((x, l) => Cons(x, l)) should be (List(1, 2, 3))
    }
  }

  "foldLeft using foldRight" should {
    "work as foldLeft" in {
      List.foldLeftOnFoldRight[Int, List[Int]](List(1, 2, 3), Nil)((l, x) => Cons(x, l)) should be (List(3, 2, 1))
    }
  }
}
