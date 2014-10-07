package chapter3

sealed trait List[+A] {
  def tail: List[A]
}

case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = Nil
}

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](list: List[A]) = list match {
    case Cons(_, xs) => xs
    case Nil => Nil
  }

  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Cons(h, xs) => Cons[A](head, xs)
    case Nil => List[A](head)
  }
}
