package chapter4

import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec

class Ex4Spec extends WordSpec with Matchers {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    @tailrec
    def convert(list: List[Option[A]],
                res: List[A] = Nil): Option[List[A]] = list match {
      case Nil if res.isEmpty => None
      case Nil => Some(res)
      case None :: _ => None
      case Some(i) :: tail => convert(tail, res :+ i)
    }

    convert(a)
  }

  "sequence" should {

    "return None for an empty List" in {
      sequence[Int](Nil) shouldBe None
    }

    "return None for a List of single None" in {
      sequence[Int](List(None)) shouldBe None
    }

    "return None if there's even a single None in the List" in {
      sequence[Int](List(Some(1), None, Some(2))) shouldBe None
    }

    "return List of all Some object's values wrapped into Some if there are no Nones" in {
      sequence[Int](List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    }
  }
}