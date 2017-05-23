package chapter4

import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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

    "return Right(Nil) for an empty List" in {
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

  def traverse[E, A, B](as: List[A])
                       (f: A => Either[E, B]): Either[E, List[B]] = {

    @tailrec
    def convert(list: List[A],
                result: Either[E, List[B]] = Right(Nil)): Either[E, List[B]] = list match {
      case Nil => result
      case head :: tail => result.map2(f(head))(_ :+ _) match {
        case newRes@Right(_) => convert(tail, newRes)
        case l@Left(_) => l
      }
    }

    convert(as)
  }

  private val f = (s: String) => Try(s.toInt) match {
    case Success(v) => Right(v)
    case Failure(e) => Left(e.getClass.getSimpleName)
  }

  "traverse" should {

    "return Right(Nil) for an empty List" in {
      traverse(Nil)(f) shouldBe Right(Nil)
    }

    "return Left if the List comprises of an item causing f to return Left" in {
      traverse(List("a"))(f) shouldBe Left("NumberFormatException")
    }

    "return Left if the List contains an item causing f to return Left" in {
      traverse(List("1", "a"))(f) shouldBe Left("NumberFormatException")
    }

    "return List of all Right object's values mapped with the 'f' " +
      "if Left isn't returned for any list's value" in {
      traverse(List("1", "2"))(f) shouldBe Right(List(1, 2))
    }
  }
}