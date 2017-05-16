package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex29Spec extends WordSpec with Matchers {

  import Tree.Implicits._

  private val sum: (Int, Tree[Int]) => Int = {
    case (acc, Leaf(v)) => acc + v
    case (acc, Branch(_, _)) => acc
  }

  "fold" should {

    "call fold function on the only leaf's value" in {
      Leaf(1).fold[Int](0)(sum) shouldBe 1
    }

    "call fold function on both tree's leafs" in {
      Branch(Leaf(1), Leaf(2)).fold[Int](0)(sum) shouldBe 3
    }

    "call fold function on all tree's leafs" in {
      val branchLevel3 = Branch(Leaf(1), Leaf(2))
      val branchLevel2LeftA = Branch(Leaf(6), branchLevel3)
      val branchLevel2LeftB = Branch(Leaf(4), Leaf(5))
      val branchLevel2Right = Branch(Leaf(2), Leaf(3))
      val branchLevel1Left = Branch(branchLevel2LeftA, branchLevel2LeftB)
      val branchLevel1Right = Branch(branchLevel2Right, Leaf(1))
      val trunk = Branch(branchLevel1Left, branchLevel1Right)

      trunk.fold[Int](0)(sum) shouldBe 24
    }
  }

  private val test2Tree =
    """
      |         t
      |    1l         1r
      | 2la  2lb     lf 2r
      |lf 3 lf lf      lf lf
      | lf fl
    """.stripMargin
}

