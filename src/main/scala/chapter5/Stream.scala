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

  def mapOnUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(
      f(h()),
      t()
    )
  }

  def takeOnUnfold(n: Int): Stream[A] = unfold(this -> n) {
    case (Empty, _) => None
    case (_, 0) => None
    case (Cons(h, t), counter) => Some(h(), t() -> (counter - 1))
  }

  def takeWhileOnUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(h, t) =>
      if (p(h())) Some(h(), t())
      else None
  }

  def zipWith[B](that: Stream[B]): Stream[(A, B)] = unfold(this -> that) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(thish, thist), Cons(thath, thatt)) => Some(
      thish() -> thath(),
      thist() -> thatt()
    )
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> that) {
    case (Empty, Empty) => None
    case (Empty, Cons(thath, thatt)) => Some(
      None -> Some(thath()),
      empty -> thatt()
    )
    case (Cons(thish, thist), Empty) => Some(
      Some(thish()) -> None,
      thist() -> empty
    )
    case (Cons(thish, thist), Cons(thath, thatt)) => Some(
      Some(thish()) -> Some(thath()),
      thist() -> thatt()
    )
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s)
    .takeWhile {
      case (_, Some(_)) => true
      case _ => false
    }
    .forAll {
      case (l, r) => l == r
    }

  def tails: Stream[Stream[A]] = unfold[Stream[A], Option[Stream[A]]](Some(this)) {
    case Some(s@Cons(_, t)) => Some(s, Some(t()))
    case Some(Empty) => Some(empty[A], None)
    case None => None
  }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {

    def intermediateResults[C](s: Stream[A]): Stream[A => B] = s match {
      case c@Cons(h, t) if c == this =>
        intermediateResults(t())
      case Cons(h, t) =>
        val newT@Cons(newH, _) = intermediateResults(t())
        cons(f(_, newH()(h())), newT)
      case Empty => cons(f(_, z), empty)
    }

    unfold[B, Option[(Stream[A], Stream[A => B])]](Some(this -> intermediateResults(this))) {
      case Some((Cons(h, t), Cons(resultsH, resultsT))) =>
        Some(resultsH()(h()), Some(t() -> resultsT()))
      case Some(_) =>
        Some(z, None)
      case None => None
    }
  }

  def scanRightOnUnfoldAndFoldRight[B](z: B)(f: (A, B) => B): Stream[B] = {

    val conditionedF: (A, => B) => B = f(_, _)

    unfold[B, Option[Stream[A]]](Some(this)) {
      case Some(s@Cons(_, t)) =>
        Some(s.foldRight(z)(conditionedF) -> Some(t()))
      case Some(_) =>
        Some(z -> None)
      case None => None
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map {
      case (nextValue, nextState) => cons(nextValue, unfold(nextState)(f))
    }.getOrElse(empty)
  }
}