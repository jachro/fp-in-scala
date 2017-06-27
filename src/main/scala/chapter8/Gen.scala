package chapter8

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

trait Gen[A]

object Gen {

  def forAll[A](gen: Gen[A])(predicate: A => Boolean): Prop = ???

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

}
