package chapter6

trait RNG {

  def nextInt: (Int, RNG)
}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (int, nextRng) => (if (int < 0) -(int % Int.MinValue) else int) -> nextRng
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (int, nextRng) => (int % Int.MaxValue).toDouble / Int.MaxValue -> nextRng
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, nextNextRng) = double(nextRng)
    (i -> d, nextNextRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), nextRng) = intDouble(rng)
    (d -> i) -> nextRng
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, nextRng1) = double(rng)
    val (d2, nextRng2) = double(nextRng1)
    val (d3, nextRng3) = double(nextRng2)
    (d1, d2, d3) -> nextRng3
  }
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }
}
