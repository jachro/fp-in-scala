package chapter8

import chapter6.RNG

case class TestRng(n: Int) extends RNG {
  def nextInt: (Int, RNG) = {
    val v = n % 5
    v -> TestRng(v + 1)
  }
}

object TestRng {

  def RNGZ5startingFrom(n: Int) = TestRng(n)

  implicit class RNGResultOps[A](gen: Gen[A]) {

    def call(times: Int)
            (implicit rng: RNG): List[A] = (1 to times).foldLeft(List.empty[A] -> rng) {
      case ((l, currentRng), _) =>
        val (v, nextRng) = gen.sample(currentRng)
        (v :: l) -> nextRng
    }._1.reverse
  }
}