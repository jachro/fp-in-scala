package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex8Spec extends WordSpec with Matchers {

  "foldRight with a list and Nil as initial value" should {
    "produce the same list as given" in {
      List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_)) should be (List(1, 2, 3))
    }
  }
}
