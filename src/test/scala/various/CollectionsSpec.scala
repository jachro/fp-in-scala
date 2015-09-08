package various

import org.scalatest.{Matchers, WordSpec}

class CollectionsSpec extends WordSpec with Matchers {

  "fold has to fold as other folds" in {
    List(1, 2, 3).fold("0 ")((i1, i2) => s"$i1$i2 ") shouldBe "0 1 2 3 "
    List().fold("0")((i1, i2) => s"$i1$i2 ") shouldBe "0"
  }

  "reduce" in {
    List(1, 2, 3).reduce((acc, i) => acc + i - 1) shouldBe 4
    List[Int]().reduceOption((acc, i) => acc + i - 1) shouldBe None
  }

  "zip" in {
    List(1, 2, 3).zip(Stream.from(1)) shouldBe List(1 -> 1, 2 -> 2, 3 -> 3)
    List(1, 2, 3).zip(List("a", "b")) shouldBe List(1 -> "a", 2 -> "b")
    List(1, 2, 3).zipAll(List("a", "b"), 0, "missing") shouldBe List(1 -> "a", 2 -> "b", 3 -> "missing")
    List(1, 2).zipAll(List("a", "b", "c"), 0, "missing") shouldBe List(1 -> "a", 2 -> "b", 0 -> "c")
  }

  "span" in {
    List(1, 2, 3).span(i => i % 2 != 0) shouldBe (List(1), List(2, 3))
  }

}
