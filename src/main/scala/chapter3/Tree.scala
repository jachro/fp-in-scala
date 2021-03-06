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

      lazy val depth: Int = {

        type TreeDepth = (Tree[A], Int)

        @tailrec
        def checkDeeper(treeToClimb: TreeDepth,
                        maybeRightTree: Option[TreeDepth] = None,
                        maybeOtherTree: Option[TreeDepth] = None,
                        maxDepth: Int = 0): Int =
          treeToClimb -> maybeRightTree match {
            case ((Leaf(_), dpth), None) => maybeOtherTree match {
              case Some(other) => checkDeeper(other, None, None, dpth + 1 max maxDepth)
              case None => dpth + 1 max maxDepth
            }
            case ((Leaf(_), dpth), Some(right)) =>
              checkDeeper(right, maybeOtherTree, None, dpth + 1 max maxDepth)
            case ((Branch(left, right), dpth), None) =>
              checkDeeper(left -> (dpth + 1), Some(right -> (dpth + 1)), maybeOtherTree, dpth + 1 max maxDepth)
            case ((Branch(left, right), dpth), previousRight) =>
              checkDeeper(left -> (dpth + 1), Some(right -> (dpth + 1)), previousRight, dpth + 1 max maxDepth)
          }

        checkDeeper(tree -> 0)
      }

      def map[B](f: A => B): Tree[B] = {

        def mapDeeper(treeToClimb: Tree[A], maybeOtherTreeToClimb: Tree[A]): Tree[B] =
          treeToClimb -> maybeOtherTreeToClimb match {
            case (Leaf(l), Leaf(r)) => Branch(Leaf(f(l)), Leaf(f(r)))
            case (Leaf(v), Branch(left, right)) => Branch(Leaf(f(v)), mapDeeper(left, right))
            case (Branch(left, right), Leaf(v)) => Branch(mapDeeper(left, right), Leaf(f(v)))
            case (Branch(ll, lr), Branch(rl, rr)) => Branch(mapDeeper(ll, lr), mapDeeper(rl, rr))
          }

        tree match {
          case Leaf(v) => Leaf(f(v))
          case Branch(l, r) => mapDeeper(l, r)
        }
      }

      def fold[B](z: B)(op: (B, Tree[A]) => B): B = {

        def foldNext(acc: B, treeToClimb: Tree[A], maybeOtherTreeToClimb: Tree[A]): B =
          treeToClimb -> maybeOtherTreeToClimb match {
            case (vl@Leaf(_), vr@Leaf(_)) => op(op(acc, vl), vr)
            case (v@Leaf(_), b@Branch(left, right)) => foldNext(op(op(acc, v), b), left, right)
            case (b@Branch(left, right), v@Leaf(_)) => foldNext(op(op(acc, b), v), left, right)
            case (bl@Branch(ll, lr), br@Branch(rl, rr)) => foldNext(foldNext(op(op(acc, bl), br), ll, lr), rl, rr)
          }

        tree match {
          case v@Leaf(_) => op(z, v)
          case b@Branch(l, r) => foldNext(op(z, b), l, r)
        }
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