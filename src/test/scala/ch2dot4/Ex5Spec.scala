package ch2dot4

import org.scalatest.{Matchers, WordSpec}

class Ex5Spec extends WordSpec with Matchers {

  "compose" should {
    "compose two functions" in {
      val composed = Ex5.compose((length: Int) => s"length is: $length", (string: String) => string.length)

      composed("two") should be ("length is: 3")
    }

    "compose two functions with tuple type" in {
      val composed = Ex5.compose((stringWithLength: (String, Int)) => s"'${stringWithLength._1}'s length is: ${stringWithLength._2}", (string: String) => (string, string.length))

      composed("two") should be ("'two's length is: 3")
    }
  }
}
