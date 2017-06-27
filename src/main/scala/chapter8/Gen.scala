package chapter8

trait Gen[A]

trait Prop {

  self =>

  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    override def check = self.check && p.check
  }
}

object Gen {

  def forAll[A](gen: Gen[A])(predicate: A => Boolean): Prop = ???

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

}
