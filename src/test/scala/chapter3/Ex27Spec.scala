package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex27Spec extends WordSpec with Matchers {

  import Tree.Implicits._

  "depth" should {

    "return 1 for a leaf" in {
      Leaf(1).depth shouldBe 1
    }

    "return 2 for a tree with two branches with a single leaf on each branch" in {
      Branch(Leaf(1), Leaf(2)).depth shouldBe 2
    }

    "return 3 for a tree with three levels of branches" in {
      val branchLevel3 = Branch(Leaf(1), Leaf(2))
      val branchLevel2LeftA = Branch(Leaf(6), branchLevel3)
      val branchLevel2LeftB = Branch(Leaf(4), Leaf(5))
      val branchLevel2Right = Branch(Leaf(2), Leaf(3))
      val branchLevel1Left = Branch(branchLevel2LeftA, branchLevel2LeftB)
      val branchLevel1Right = Branch(branchLevel2Right, Leaf(1))
      val trunk = Branch(branchLevel1Left, branchLevel1Right)
      trunk.depth shouldBe 5
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

