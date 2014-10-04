package chapter2

import org.scalatest.{Matchers, WordSpec}

class Ex3Spec extends WordSpec with Matchers {

  "curry" should {
    "return function requiring one arg and another one" in {
      val curried = Ex3.curry((s1: String, s2: String) => s"$s1 $s2")
      curried("a")("b") should be ("a b")

      val firstReturn = curried("d")
      firstReturn("e") should be ("d e")
    }
  }
}
