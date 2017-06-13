package chapter6

trait RNG {

  def nextInt: (Int, RNG)
}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, nextRng) => 0 -> nextRng
    case (int, nextRng) if int < 0 => -int -> nextRng
    case (int, nextRng) => int -> nextRng
  }

  def double(rng: RNG): (Double, RNG) = rng.nextInt match {
    case (int, nextRng) =>
      val numerator = int % Int.MaxValue
      val positiveNumerator = if (numerator < 0) -numerator else numerator
      positiveNumerator.toDouble / Int.MaxValue -> nextRng
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
