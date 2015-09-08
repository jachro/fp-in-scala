package tests

import org.scalatest.{Matchers, WordSpec}

class BarclaysSpec extends WordSpec with Matchers {

  "sumUpColumns" should {
    "return an Array of sums of columns" in {
      val matrix = Array(Array(1, 2, 3), Array(1, 2, 3))
      Barclays.sumUpColumns(matrix) shouldBe Array(2, 4, 6)
    }

    "return a List of sums of columns" in {
      val matrix = Array(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))
      Barclays.sumUpColumnsWithList(matrix) shouldBe List(3, 6, 9)
    }
  }

  "functions with currying" in {
    val f: (Int) => (Int) => Int =
      (v1: Int) => (v2: Int) => v1 + v2
    val partial = f(1)
    partial(2) shouldBe 3

    def defF(v1: Int)(v2: Int): Int = v1 + v2
    val partialDefF = defF(1) _
    partialDefF(2) shouldBe 3

    val partialPartial = f(1) {2}
    partialPartial shouldBe 3

    val anotherPartial: PartialFunction[Int, Int] = {
      case 2 => 0
      case v => v
    }
    f(1)(anotherPartial(2)) shouldBe 1
    f(1)(anotherPartial(3)) shouldBe 4
  }


}
