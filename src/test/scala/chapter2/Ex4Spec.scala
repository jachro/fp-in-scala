package chapter2

import org.scalatest.{Matchers, WordSpec}

class Ex4Spec extends WordSpec with Matchers {

  "uncurry" should {
    "return function requiring two args" in {
      val uncurried = Ex4.uncurry((s: String) => (t: String) => s"$s $t")
      uncurried("a", "b") should be ("a b")
    }
  }
}
