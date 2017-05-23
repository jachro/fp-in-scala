package chapter4

import org.scalatest.{Matchers, WordSpec}

class Ex1Spec extends WordSpec with Matchers {

  "map" should {

    "return None if called on None" in {
      None.map(_ => 1) shouldBe None
    }

    "return Some with converted value if called on Some" in {
      Some(1).map(_.toString) shouldBe Some("1")
    }
  }

  "flatMap" should {

    "return None if called on None" in {
      None.flatMap(_ => Some(1)) shouldBe None
    }

    "return Some with converted value if called on Some" in {
      Some(1).flatMap(v => Some(v.toString)) shouldBe Some("1")
    }

    "return None if called on Some but the given function returns None" in {
      Some(1).flatMap(_ => None) shouldBe None
    }
  }

  "getOrElse" should {

    "return the Some's value" in {
      Some(1).getOrElse("1") shouldBe 1
    }

    "return default value for None" in {
      None.getOrElse("1") shouldBe "1"
    }
  }

  "orElse" should {

    "return the Some's value" in {
      Some(1).orElse(Some("1")) shouldBe Some(1)
    }

    "return Some passed as an 'orElse' arg when invoked on None" in {
      None.orElse(Some("1")) shouldBe Some("1")
    }

    "return None passed as an 'orElse' arg when invoked on None" in {
      None.orElse(None) shouldBe None
    }
  }

  "filter" should {

    "return None for Some if Some's value doesn't satisfy the condition" in {
      Some(1).filter(_ > 1) shouldBe None
    }

    "return the Some when Some's value satisfies the condition" in {
      Some(1).filter(_ == 1) shouldBe Some(1)
    }

    "return None for None" in {
      None.filter(_ => true) shouldBe None
    }
  }
}