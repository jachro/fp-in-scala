package chapter8

import java.lang.Math.abs

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

  def listOfNOnFlatMap[A](n: Int, g: Gen[A]): Gen[List[A]] = {

    def listGen(l: List[A]): Gen[List[A]] = flatMap(g) {
      case v if l.size == n - 1 => unit((v :: l).reverse)
      case v => listGen(v :: l)
    }

    listGen(List.empty)
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

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {

    def startGen(g: Gen[A]) = flatMap(g) {
      case _ if g == g1 => g2
      case _ => g2
    }

    startGen(g1)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {

    val (gen1, gen1Weight) = g1
    val (gen2, gen2Weight) = g2

    require(gen1Weight + gen2Weight == 1, "Given weights have to sum up to 1")

    var count1, count2 = 0L

    def sum: Double = count1 + count2

    def ratio(s: Long): Double = if (sum != 0) s.toDouble / sum else 0D

    def distance(c: Long, weight: Double) = abs(weight - ratio(c))

    def gen(g: Gen[A]): Gen[A] = flatMap(g) {

      _ => {

        println(if (g == gen1) "gen1" else "gen2")
        if (g == gen1) count1 += 1
        else count2 += 1

        println(s"c1: $count1; c2: $count2; sum: $sum; f: ${distance(count1, gen1Weight)}; s: ${distance(count2, gen2Weight)}")

        if (distance(count1, gen1Weight) > distance(count2, gen2Weight)) {
          println("gen1")
          gen1
        }
        else {
          println("gen2")
          gen2
        }
      }
    }

    gen {

      println(s"c1: $count1; c2: $count2; sum: $sum; f: ${distance(count1, gen1Weight)}; s: ${distance(count2, gen2Weight)}")

      if (distance(count1, gen1Weight) > distance(count2, gen2Weight)) gen1
      else gen2
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
