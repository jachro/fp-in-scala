package various

import org.scalatest.{Matchers, WordSpec}

class ConstrainingAllowedInstancesSpec extends WordSpec with Matchers {

  "MagicBox.get" should {
    "allow to get the val as a String" in {
      val box = MagicBox("a")
      box.get[String] shouldBe "a"
    }

    "allow to get the val as an Int" in {
      val box = MagicBox("1")
      box.get[Int] shouldBe 1
    }

    "allow to get the val as a wrapped Int" in {
      val box = MagicBox("1")
      box.get[SomeWrapper[Int]] shouldBe SomeWrapper(1)
    }
  }
}
