package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex26Spec extends WordSpec with Matchers {

  import Tree.Implicits._

  "max" should {

    "return leaf's value for a single Leaf" in {
      Leaf(1).max shouldBe 1
    }

    "return value of a leaf with greater value - a case of a tree with two leaves" in {
      Branch(Leaf(1), Leaf(2)).max shouldBe 2
    }

    "return value of a leaf with greatest value - a case of a complicated tree" in {
      val branchLevel3 = Branch(Leaf(1), Leaf(2))
      val branchLevel2LeftA = Branch(Leaf(6), branchLevel3)
      val branchLevel2LeftB = Branch(Leaf(4), Leaf(5))
      val branchLevel2Right = Branch(Leaf(2), Leaf(3))
      val branchLevel1Left = Branch(branchLevel2LeftA, branchLevel2LeftB)
      val branchLevel1Right = Branch(branchLevel2Right, Leaf(1))
      val trunk = Branch(branchLevel1Left, branchLevel1Right)
      trunk.max shouldBe 6
    }
  }
}
