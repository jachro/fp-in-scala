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
}