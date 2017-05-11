package chapter3

import org.scalatest.{Matchers, WordSpec}

class Ex25Spec extends WordSpec with Matchers {

  import Tree.Implicits._

  "sizeNonTailRecursive" should {

    "return 1 for a single Leaf" in {
      Leaf("a").sizeNonTailRecursive shouldBe 1
    }

    "return 3 for a branch with two leaves" in {
      Branch(Leaf("1a"), Leaf("1b")).sizeNonTailRecursive shouldBe 3
    }

    "return size of more complicated tree" in {
      val branchLevel3 = Branch(Leaf("3a"), Leaf("3b"))
      val branchLevel2LeftA = Branch(Leaf("2a"), branchLevel3)
      val branchLevel2LeftB = Branch(Leaf("2b"), Leaf("2c"))
      val branchLevel2Right = Branch(Leaf("2d"), Leaf("2ee"))
      val branchLevel1Left = Branch(branchLevel2LeftA, branchLevel2LeftB)
      val branchLevel1Right = Branch(branchLevel2Right, Leaf("1a"))
      val trunk = Branch(branchLevel1Left, branchLevel1Right)
      trunk.sizeNonTailRecursive shouldBe 15
    }
  }

  "size" should {

    "return 1 for a single Leaf" in {
      Leaf("a").size shouldBe 1
    }

    "return 3 for a branch with two leaves" in {
      Branch(Leaf("1a"), Leaf("1b")).size shouldBe 3
    }

    "return size of more complicated tree" in {
      val branchLevel3 = Branch(Leaf("3a"), Leaf("3b"))
      val branchLevel2LeftA = Branch(Leaf("2a"), branchLevel3)
      val branchLevel2LeftB = Branch(Leaf("2b"), Leaf("2c"))
      val branchLevel2Right = Branch(Leaf("2d"), Leaf("2ee"))
      val branchLevel1Left = Branch(branchLevel2LeftA, branchLevel2LeftB)
      val branchLevel1Right = Branch(branchLevel2Right, Leaf("1a"))
      val trunk = Branch(branchLevel1Left, branchLevel1Right)
      trunk.size shouldBe 15
    }
  }
}
