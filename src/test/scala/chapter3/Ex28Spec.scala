package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex28Spec extends WordSpec with Matchers {

  import Tree.Implicits._

  private def leafsToString[A]: A => String = _.toString

  "map" should {

    "call mapping function on the only leaf's value" in {
      Leaf(1) map leafsToString shouldBe Leaf("1")
    }

    "call mapping function on both tree's leafs" in {
      Branch(Leaf(1), Leaf(2)) map leafsToString shouldBe Branch(Leaf("1"), Leaf("2"))
    }

    "call mapping function on all tree's leafs" in {
      val branchLevel3 = Branch(Leaf(1), Leaf(2))
      val branchLevel2LeftA = Branch(Leaf(6), branchLevel3)
      val branchLevel2LeftB = Branch(Leaf(4), Leaf(5))
      val branchLevel2Right = Branch(Leaf(2), Leaf(3))
      val branchLevel1Left = Branch(branchLevel2LeftA, branchLevel2LeftB)
      val branchLevel1Right = Branch(branchLevel2Right, Leaf(1))
      val trunk = Branch(branchLevel1Left, branchLevel1Right)

      val expBranchLevel3 = Branch(Leaf("1"), Leaf("2"))
      val expBranchLevel2LeftA = Branch(Leaf("6"), expBranchLevel3)
      val expBranchLevel2LeftB = Branch(Leaf("4"), Leaf("5"))
      val expBranchLevel2Right = Branch(Leaf("2"), Leaf("3"))
      val expBranchLevel1Left = Branch(expBranchLevel2LeftA, expBranchLevel2LeftB)
      val expBranchLevel1Right = Branch(expBranchLevel2Right, Leaf("1"))
      val expTrunk = Branch(expBranchLevel1Left, expBranchLevel1Right)

      trunk map leafsToString shouldBe expTrunk
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

