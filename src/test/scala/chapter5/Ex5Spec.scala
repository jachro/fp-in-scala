package chapter5

import org.scalatest.{Matchers, WordSpec}

class Ex5Spec extends WordSpec with Matchers {

  "takeWhile implemented with foldRight" should {

    "return an empty Stream if takeWhile evaluates to false for the first element" in {
      Stream(1, 2, 3).takeWhileOnFoldRight(_ < 1) shouldBe Stream.empty[Int]
    }

    "return an empty Stream if called on an empty Stream" in {
      Stream.empty[Int].takeWhileOnFoldRight(_ < 3) shouldBe Stream.empty[Int]
    }

    "return an empty Stream if the first item in the Stream does not match" in {
      Stream(3, 1).takeWhileOnFoldRight(_ < 3) shouldBe Stream.empty[Int]
    }

    "return a Stream with all the elements of the given Stream " +
      "if all of them match the predicate" in {
      Stream(1, 2, 3, 1).takeWhileOnFoldRight(_ < 4).toList shouldBe List(1, 2, 3, 1)
    }

    "return a Stream with only the first elements of the Stream satisfying the predicate " +
      "even if there are matching elements after one not matching" in {
      Stream(1, 2, 3, 1).takeWhileOnFoldRight(_ < 3).toList shouldBe List(1, 2)
    }

    "return a Stream with only the first n elements of the Stream satisfying the predicate " +
      "even if there are matching elements after one not matching - different case" in {
      Stream(1, 2, 3, 2, 1, 4, 1).takeWhileOnFoldRight(_ < 3).toList shouldBe List(1, 2)
    }
  }

  private implicit class StreamOps[A](s: Stream[A]) {

    def takeWhileOnFoldRight(f: A => Boolean): Stream[A] = s.foldRight(Stream.empty[A]) {
      case (item, result) if f(item) => Cons(() => item, () => result)
      case _ => Stream.empty[A]
    }
  }

}