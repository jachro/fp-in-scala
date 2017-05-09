package chapter3

import scala.annotation.tailrec

sealed trait List[+A] {
  def tail: List[A]
}

case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = Nil
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {

  override def toString: String = {

    def headToString(converted: String,
                     toConvert: List[A]): String =
      toConvert match {
        case Nil =>
          converted
        case Cons(h, t) =>
          if (converted.isEmpty)
            headToString(h.toString, t)
          else
            headToString(converted + ", " + h, t)
      }

    headToString("", this)
  }
}

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
    case Cons(_, xs) => Cons[A](head, xs)
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
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, xs) => Cons(h, init[A](xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(
    (a, b) => a match {
      case 0 => return 0.0
      case x => x * b
    }
  )

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, xs) => foldLeft(xs, f(z, h))(f)
  }

  def lengthOnFoldLeft(list: List[Int]) =
    foldLeft(list, 0)((tot, x) => tot + 1)

  def sumOnFoldLeft(list: List[Int]) =
    foldLeft(list, 0)((sum, x) => sum + x)

  def productOnFoldLeft(list: List[Int]) =
    foldLeft(list, 1)((prod, x) => prod * x)

  def reverse[A](l: List[A]): List[A] =
    foldLeft[A, List[A]](l, Nil)((rev, x) => Cons[A](x, rev))

  def foldRightOnFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val rev = foldLeft[A, List[A]](l, Nil)((r, h) => Cons(h, r))
    foldLeft[A, B](rev, z)((b, a) => f(a, b))
  }

  def foldLeftOnFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    val rev = foldRight[A, List[A]](l, Nil)((i, acc) => acc + i)
    foldRight[A, B](rev, z)((a, b) => f(b, a))
  }

  def append[A](list: List[A], item: A) =
    foldRight(list, List(item))((currentItem, newList) => Cons(currentItem, newList))

  def concatenate[A](list: List[List[A]]): List[A] = {

    def appendLists(list1: List[A], list2: List[A]) =
      foldLeft[A, List[A]](list2, list1)((newList1, list2Item) => Cons(list2Item, newList1))

    val flat = foldLeft[List[A], List[A]](list, Nil) { (concatenated, item) =>
      item match {
        case Nil => concatenated
        case Cons(head, Nil) => Cons(head, concatenated)
        case Cons(head, tail) => appendLists(Cons(head, concatenated), tail)
      }
    }

    reverse(flat)
  }

  def increment(list: List[Int]): List[Int] = {

    def unstash(stashed: List[Int], newList: List[Int]): List[Int] = stashed match {
      case Nil => newList
      case Cons(head, Nil) => Cons(head, newList)
      case Cons(head, tail) => unstash(tail, Cons(head, newList))
    }

    def appendAsLast(list: List[Int], item: Int, stashed: List[Int] = Nil): List[Int] = list match {
      case Nil => Cons(item, Nil)
      case Cons(head, Nil) => unstash(Cons(head, stashed), Cons(item, Nil))
      case Cons(head, tail) => appendAsLast(tail, item, Cons(head, stashed))
    }

    foldLeft[Int, List[Int]](list, Nil) { (incremented, item) =>
      appendAsLast(incremented, item + 1)
    }
  }

  def mapDoublesToStrings(list: List[Double]): List[String] =
    reverse(mapToStrings(list, Nil))

  private def mapToStrings(toMap: List[_], alreadyMapped: List[String]): List[String] =
    toMap match {
      case Nil =>
        alreadyMapped
      case Cons(head, tail) =>
        mapToStrings(tail, Cons(head.toString, alreadyMapped))
    }

  def map[A, B](list: List[A])(f: (A) => B): List[B] = {

    @tailrec
    def map[A, B](itemsToMap: List[A], itemsMapped: List[B])(f: A => B): List[B] =
      itemsToMap match {
        case Nil => itemsMapped
        case Cons(head, tail) => map(tail, itemsMapped + f(head))(f)
      }

    map(list, Nil)(f)
  }

  def filter[A](list: List[A])(predicate: A => Boolean): List[A] = {

    @tailrec
    def filter(toBeFiltered: List[A], result: List[A]): List[A] = toBeFiltered match {
      case Nil => result
      case Cons(head, tail) if predicate(head) => filter(tail, result + head)
      case Cons(_, tail) => filter(tail, result)
    }

    filter(list, Nil)
  }

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {

    @tailrec
    def flatMap(input: List[A], output: List[B])
               (f: A => List[B]): List[B] = input match {
      case Nil => output
      case Cons(head, Nil) => output ++ f(head)
      case Cons(head, tail) => flatMap(tail, output ++ f(head))(f)
    }

    flatMap(list, Nil)(f)
  }

  def filterOnFlatMap[A](list: List[A])
                        (predicate: A => Boolean): List[A] = flatMap(list) {
    case i if predicate(i) => Cons(i, Nil)
    case _ => Nil
  }

  def merge[A](first: List[A], second: List[A])
              (combine: (A, A) => A): List[A] = {

    def mergeF(first: List[A], second: List[A],
               outcome: List[A] = Nil): List[A] =
      first -> second match {
        case (Nil, Nil) => outcome
        case (Cons(h, tail), Nil) => mergeF(tail, Nil, outcome + h)
        case (Nil, Cons(h, tail)) => mergeF(Nil, tail, outcome + h)
        case (Cons(f, firstTail), Cons(s, secondTail)) => mergeF(firstTail, secondTail, outcome + combine(f, s))
      }

    mergeF(first, second)
  }

  def zipWith[A, B, C](first: List[A], second: List[B])
                      (combine: (Option[A], Option[B]) => C): List[C] = {

    def mergeF(first: List[A], second: List[B],
               outcome: List[C] = Nil): List[C] =
      first -> second match {
        case (Nil, Nil) => outcome
        case (Cons(h, tail), Nil) => mergeF(tail, Nil, outcome + combine(Some(h), None))
        case (Nil, Cons(h, tail)) => mergeF(Nil, tail, outcome + combine(None, Some(h)))
        case (Cons(f, firstTail), Cons(s, secondTail)) => mergeF(firstTail, secondTail, outcome + combine(Some(f), Some(s)))
      }

    mergeF(first, second)
  }

  private implicit class ListOps[A](list: List[A]) {

    def +(elem: A): List[A] = addToTheEnd(list, elem)

    @tailrec
    final def ++(another: List[A]): List[A] = another match {
      case Nil => list
      case Cons(head, Nil) => addToTheEnd(list, head)
      case Cons(head, tail) => addToTheEnd(list, head) ++ tail
    }


    @tailrec
    private def addToTheEnd(list: List[A], item: A, stacked: List[A] = Nil): List[A] =
      list match {
        case Nil => Cons(item, Nil)
        case Cons(last, Nil) => takeFromTheStack(stacked, Cons(last, Cons(item, Nil)))
        case Cons(head, tail) => addToTheEnd(tail, item, Cons(head, stacked))
      }

    @tailrec
    private def takeFromTheStack(stacked: List[A], list: List[A]): List[A] =
      stacked match {
        case Nil => list
        case Cons(head, Nil) => Cons(head, list)
        case Cons(head, tail) => takeFromTheStack(tail, Cons(head, list))
      }
  }

}
