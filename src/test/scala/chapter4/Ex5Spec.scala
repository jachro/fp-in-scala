package chapter4

import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class Ex5Spec extends WordSpec with Matchers {

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    @tailrec
    def convert(list: List[A],
                res: List[B] = Nil): Option[List[B]] = list match {
      case Nil if res.isEmpty => None
      case Nil => Some(res)
      case head :: tail => f(head) match {
        case None => None
        case Some(v) => convert(tail, res :+ v)
      }
    }

    convert(a)
  }

  private val f = (s: String) => Try(s.toInt) match {
    case Success(v) => Some(v)
    case Failure(_) => None
  }

  "traverse" should {

    "return None for an empty List" in {
      traverse[String, Int](Nil)(f) shouldBe None
    }

    "return None if the List comprises of an item causing f to return None" in {
      traverse[String, Int](List("a"))(f) shouldBe None
    }

    "return None if the List contains an item causing f to return None" in {
      traverse[String, Int](List("1", "a"))(f) shouldBe None
    }

    "return List of all Some object's values wrapped into Some if there are no Nones returned from the f" in {
      traverse[String, Int](List("1", "2"))(f) shouldBe Some(List(1, 2))
    }
  }
}