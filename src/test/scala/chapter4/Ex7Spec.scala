package chapter4

import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec

class Ex7Spec extends WordSpec with Matchers {

  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {

    @tailrec
    def convert(list: List[Either[E, A]],
                result: Either[E, List[A]] = Right(Nil)): Either[E, List[A]] = list match {
      case Nil => Right(Nil)
      case Left(v) :: _ => Left(v)
      case Right(v) :: Nil => result.map(_ :+ v)
      case Right(v) :: tail => convert(tail, result.map(_ :+ v))
    }

    convert(a)
  }

  "sequence" should {

    "return Nil for an empty List" in {
      sequence(Nil) shouldBe Right(Nil)
    }

    "return Left for a List of single Left" in {
      sequence(List(Left("err1"))) shouldBe Left("err1")
    }

    "return the first Left if there are in the List" in {
      sequence(List(Right(1), Left("err1"), Left("err2"), Right(2), Left("err3"))) shouldBe Left("err1")
    }

    "return List of all Right object's values wrapped into Right if there are no Lefts" in {
      sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    }
  }
}