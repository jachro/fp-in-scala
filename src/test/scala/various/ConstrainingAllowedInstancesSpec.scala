package various

import org.scalatest.{Matchers, WordSpec}

class ConstrainingAllowedInstancesSpec extends WordSpec with Matchers {

  "get" should {
    "allow to get Int" in {
      val box = MagicBox()
      MagicBox.get
    }
  }
}
