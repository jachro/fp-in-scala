package chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def toList: List[A] = {

    @tailrec
    def convertToList(stream: Stream[A],
                      converted: List[A] = Nil): List[A] = stream match {
      case Empty => converted
      case Cons(hd, ta) => convertToList(ta(), converted :+ hd())
    }

    convertToList(this)
  }

  def take(n: Int): Stream[A] = {

    def takeMoreOrEmpty(stream: Stream[A],
                        counter: Int): Stream[A] = stream match {
      case Empty => empty
      case Cons(hd, ta) if counter > 0 => Cons(hd, () => takeMoreOrEmpty(ta(), counter - 1))
      case Cons(_, _) => empty
    }

    takeMoreOrEmpty(this, n)
  }

  def drop(n: Int): Stream[A] = {

    @tailrec
    def dropOrTake(stream: Stream[A],
                   counter: Int): Stream[A] = stream match {
      case Empty => empty
      case Cons(_, ta) if counter > 0 => dropOrTake(ta(), counter - 1)
      case c@Cons(_, _) => c
    }

    dropOrTake(this, n)
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A,
                    t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {

    lazy val hd = h
    lazy val ta = t

    Cons(() => hd, () => ta)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](items: A*): Stream[A] =
    if (items.isEmpty) empty
    else cons(items.head, apply(items.tail: _*))
}
