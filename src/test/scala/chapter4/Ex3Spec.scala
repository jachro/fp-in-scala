package chapter4

import org.scalatest.{Matchers, WordSpec}

class Ex3Spec extends WordSpec with Matchers {

  private val f = (a: Int, b: Int) => s"$a -> $b"

  def map2[A,B,C](a: Option[A], b: Option[B])
                 (f: (A, B) => C): Option[C] = for {
    av <- a
    bv <- b
  } yield f(av, bv)

  "map2" should {

    "return None if the first argument is None" in {
      map2(None, Some(1))(f) shouldBe None
    }

    "return None if the second argument is None" in {
      map2(None, Some(1))(f) shouldBe None
    }

    "return None if both the arguments are None" in {
      map2(None, Some(1))(f) shouldBe None
    }

    "return result of applying the given function on the args wrapped in a Some" in {
      map2(Some(1), Some(2))(f) shouldBe Some("1 -> 2")
    }
  }
}