package chapter8

import chapter6.RNG
import chapter6.State.State

object Prop {

  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {

  self =>

  import Prop._

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = new Prop {

    override def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      self.check flatMap {
        count =>
          p.check fold(
            failure => Left(failure._1 -> (count + failure._2)),
            pCount => Right(count + pCount)
          )
      }
  }
}

case class Gen[A](sample: State[RNG, A])

object Gen {

  import RNG._

  def forAll[A](gen: Gen[A])(predicate: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {

    def intFromRange: Rand[Int] =
      flatMap[Int, Int](nonNegativeInt) {
        case i if i >= start && i < stopExclusive => RNG => i -> RNG
        case i => intFromRange
      }

    Gen[Int](intFromRange)
  }

  def unit[A](v: => A): Gen[A] = Gen[A](RNG => v -> RNG)

  def boolean: Gen[Boolean] = Gen[Boolean] {
    RNG => {
      val (i, nextRng) = RNG.nextInt
      (i % 2 == 0) -> nextRng
    }
  }

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

}
