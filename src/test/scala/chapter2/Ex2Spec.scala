package chapter2

import org.scalatest.{Matchers, WordSpec}

class Ex2Spec extends WordSpec with Matchers {

  "isSorted" should {
    "say yes if given ints are in natural order" in {
      Ex2.isSorted(Array(1, 2, 3), (n1: Int, n2: Int) => n1 <= n2) should be(true)
    }

    "say yes if given two ints are in natural order" in {
      Ex2.isSorted(Array(2, 3), (n1: Int, n2: Int) => n1 <= n2) should be(true)
    }

    "say yes if given one element array" in {
      Ex2.isSorted(Array(2), (n1: Int, n2: Int) => n1 <= n2) should be(true)
    }

    "say no if given ints are not in natural order" in {
      Ex2.isSorted(Array(1, 3, 2), (n1: Int, n2: Int) => n1 <= n2) should be(false)
    }

    "say no if given two ints are in natural order" in {
      Ex2.isSorted(Array(3, 2), (n1: Int, n2: Int) => n1 <= n2) should be(false)
    }

    "say yes if every next string in the array is longer than the previous" in {
      Ex2.isSorted(Array("a", "aa", "aaa"), (n1: String, n2: String) => n1.length <= n2.length) should be(true)
    }
  }
}
