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

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(hd, ta) if n > 0 => Cons(hd, () => ta().take(n - 1))
    case Cons(_, _) => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(_, ta) if n > 0 => ta().drop(n - 1)
    case c@Cons(_, _) => c
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case Cons(_, _) => empty
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(hd, ta) => f(hd(), ta().foldRight(z)(f))
    case Empty => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  lazy val headOption: Option[A] = foldRight(None: Option[A]) {
    case (i, _) => Some(i)
  }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) {
    case (i, res) => cons(f(i), res)
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A]) {
    case (i, res) if p(i) => cons(i, res)
    case (_, res) => res
  }

  def append[B >: A](i: B): Stream[B] = foldRight[Stream[B]](cons(i, empty[B])) {
    case (item, res) => cons(item, res)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) {
    case (i, res) => f(i).foldRight(res) {
      case (ci, ar) => cons(ci, ar)
    }
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(start: Int): Stream[Int] = cons(start, from(start + 1))

  def fibs: Stream[Int] = {

    def next(prvs: Int, nxt: Int): Stream[Int] = cons(prvs, next(nxt, prvs + nxt))

    next(0, 1)
  }
}