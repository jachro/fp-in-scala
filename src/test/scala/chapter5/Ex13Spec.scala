package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex13Spec extends WordSpec with Matchers {

  "unfold" should {

    "allow to be used to build the 'map' method" in {

      Stream.empty[Int].mapOnUnfold(_.toString) shouldBe Stream.empty[Int]

      Stream(1, 2, 3).mapOnUnfold(_.toString).toList shouldBe List("1", "2", "3")
    }

    "allow to be used to build the 'take' method" in {

      Stream.empty[Int].takeOnUnfold(2) shouldBe Stream.empty[Int]

      Stream(1, 2, 3).takeOnUnfold(2).toList shouldBe List(1, 2)
    }

    "allow to be used to build the 'zipWith' method" in {

      Stream.empty[Int].zipWith(Stream(1, 2)) shouldBe Stream.empty[Int]

      Stream(1, 2, 3).zipWith(Stream("1", "2", "3")).toList shouldBe List(1 -> "1", 2 -> "2", 3 -> "3")

      Stream(1, 2).zipWith(Stream("1", "2", "3")).toList shouldBe List(1 -> "1", 2 -> "2")

      Stream(1, 2, 3).zipWith(Stream("1", "2")).toList shouldBe List(1 -> "1", 2 -> "2")
    }

    "allow to be used to build the 'zipAll' method" in {

      Stream.empty[Int].zipAll(Stream.empty[String]) shouldBe Stream.empty[Int]

      Stream.empty[Int].zipAll(Stream(1, 2)).toList shouldBe List(None -> Some(1), None -> Some(2))

      Stream(1, 2).zipAll(Stream.empty[Int]).toList shouldBe List(Some(1) -> None, Some(2) -> None)

      Stream(1, 2, 3).zipAll(Stream("1", "2", "3")).toList shouldBe List(Some(1) -> Some("1"), Some(2) -> Some("2"), Some(3) -> Some("3"))

      Stream(1, 2).zipAll(Stream("1", "2", "3")).toList shouldBe List(Some(1) -> Some("1"), Some(2) -> Some("2"), None -> Some("3"))

      Stream(1, 2, 3).zipAll(Stream("1", "2")).toList shouldBe List(Some(1) -> Some("1"), Some(2) -> Some("2"), Some(3) -> None)
    }
  }
}