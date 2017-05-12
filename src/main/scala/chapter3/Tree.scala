package chapter3

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  object Implicits {

    implicit class TreeOps[A](tree: Tree[A]) {

      lazy val sizeNonTailRecursive: Int = {

        def fold(acc: Int, treeToClimb: Tree[A]): Int = treeToClimb match {
          case Leaf(_) => acc + 1
          case Branch(left, right) => 1 + fold(acc, left) + fold(acc, right)
        }

        fold(0, tree)
      }

      lazy val size: Int = {

        @tailrec
        def fold(acc: Int, treeToClimb: Tree[A], maybeOtherTreeToClimb: Option[Tree[A]] = None): Int =
          treeToClimb -> maybeOtherTreeToClimb match {
            case (Leaf(_), None) => acc + 1
            case (Leaf(_), Some(other)) => fold(acc + 1, other, None)
            case (Branch(left, right), None) => fold(acc + 1, left, Some(right))
            case (Branch(left, right), Some(other)) => fold(acc, left, Some(Branch(right, other)))
          }

        fold(0, tree)
      }
    }

    implicit class IntTreeOps(tree: Tree[Int]) {

      lazy val max: Int = {

        def findMaxOfOne(previous: Int, tree: Tree[Int]): Int =
          tree match {
            case Leaf(v) => v max previous
            case Branch(l, r) => findMaxOfTwo(previous, l, r)
          }

        @tailrec
        def findMaxOfTwo(previous: Int, tree: Tree[Int], otherTree: Tree[Int]): Int =
          tree -> otherTree match {
            case (Leaf(l), Leaf(r)) => previous max l max r
            case (Leaf(leaf), Branch(l, r)) => findMaxOfTwo(previous max leaf, l, r)
            case (Branch(l, r), Leaf(leaf)) => findMaxOfTwo(previous max leaf, l, r)
            case (Branch(l, r), b2@Branch(_, _)) => findMaxOfTwo(previous, l, Branch(r, b2))
          }

        findMaxOfOne(Int.MinValue, tree)
      }
    }

  }

}