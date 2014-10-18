package chapter3

import scala.annotation.tailrec

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

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ if n == 0 => l
    case Nil => Nil
  }

  def dropWhile[A](l: List[A], f: (A) => Boolean): List[A] = l match {
    case Cons(h, xs) if f(h) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](list: List[A]): List[A] = list match {
    case Cons(h, Nil) => Nil
    case Cons(h, xs) => Cons(h, init[A](xs))
    case Nil => Nil
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)((a, b) => {
    println(s"tail value: $a")
    a match {
      case 0 => return 0.0
      case x => a * b}
    }
  )

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((a,b) => b + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, xs) => foldLeft(xs, f(z, h))(f)
  }

}
