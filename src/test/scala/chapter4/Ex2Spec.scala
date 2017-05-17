package chapter4

import org.scalatest.{Matchers, WordSpec}

class Ex2Spec extends WordSpec with Matchers {

  private def variance(xs: Seq[Double]): Option[Double] = {

    def mean(seq: Seq[Double]): Option[Double] = seq match {
      case Nil => None
      case s => Some(s.sum / s.size)
    }

    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))
  }

  "variance" should {

    "return None an empty seq" in {
      variance(Nil) shouldBe None
    }

    "return variance for non-empty seq" in {
      variance(Seq(1, 2, 3)) shouldBe Some(2D / 3D)
    }
  }
}