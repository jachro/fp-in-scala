package chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def toList: List[A] = {

    @tailrec
    def convert(stream: Stream[A],
                converted: List[A] = Nil): List[A] = stream match {
      case Empty => converted
      case Cons(hd, ta) => convert(ta(), converted :+ hd())
    }

    convert(this)
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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
