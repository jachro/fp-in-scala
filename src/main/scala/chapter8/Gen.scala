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

  import RNG.{flatMap => flatMapRng, nonNegativeInt => nonNegativeIntRng, _}

  def forAll[A](gen: Gen[A])(predicate: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {

    def intFromRange: Rand[Int] =
      flatMapRng[Int, Int](nonNegativeIntRng) {
        case i if i >= start && i < stopExclusive => rng => i -> rng
        case _ => intFromRange
      }

    Gen[Int](intFromRange)
  }

  def unit[A](v: => A): Gen[A] = Gen[A](rng => v -> rng)

  def nonNegativeInt: Gen[Int] = Gen(nonNegativeIntRng)

  def boolean: Gen[Boolean] = Gen[Boolean] {
    rng => {
      val (i, nextRng) = rng.nextInt
      (i % 2 == 0) -> nextRng
    }
  }

  def listOf[A](g: Gen[A]): Gen[List[A]] = flatMap(choose(0, Int.MaxValue)) {
    case 0 => unit(List.empty[A])
    case n => listOfN(n, g)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen[List[A]] { rng =>

    val (r, nextRng) = (1 to n).foldLeft(List.empty[A] -> rng) {
      case ((l, currentRng), _) =>
        val (v, nextRng) = g.sample(currentRng)
        (v :: l) -> nextRng
    }

    r.reverse -> nextRng
  }

  def map2[A, B, C](gA: Gen[A], gB: Gen[B])
                   (f: (A, B) => C): Gen[C] = Gen[C] {
    rng => {

      val (a, nRng) = gA.sample(rng)
      val (b, nnRng) = gB.sample(nRng)

      (f(a, b), nnRng)
    }
  }

  def map[A, B](g: Gen[A])
               (f: A => B): Gen[B] = flatMap(g) {
    v => unit(f(v))
  }

  def flatMap[A, B](g: Gen[A])
                   (f: A => Gen[B]): Gen[B] = Gen[B] {
    rng => {

      val (v, nRng) = g.sample(rng)

      val Gen(nextSample) = f(v)

      nextSample(nRng)
    }
  }

  def tuple[A](g: Gen[A]): Gen[(A, A)] = map2(g, g)(_ -> _)

  def option[A](g: Gen[A]): Gen[Option[A]] = map2(g, boolean) {
    case (a, true) => Some(a)
    case (a, false) => None
  }

  def string: Gen[String] = map(listOf(choose(32, 128))) {
    _.map(_.toChar).mkString
  }
}
