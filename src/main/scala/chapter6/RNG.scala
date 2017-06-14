package chapter6

trait RNG {

  import RNG._

  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

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

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count > 0) {
      val (int, nextRng) = rng.nextInt
      val (intsList, nextNextRng) = ints(count - 1)(nextRng)
      (int :: intsList) -> nextNextRng
    } else {
      Nil -> rng
    }

  def unit[A](a: A): Rand[A] =
    rnd => (a, rnd)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, newRng) = s(rng)
    (f(a), newRng)
  }

  lazy val doubleOnMap: Rand[Double] = rng => {

    val (i, nextRng) = nonNegativeInt(rng)

    (i % Int.MaxValue).toDouble / Int.MaxValue -> nextRng
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {

    val (i, rngI) = ra(rng)
    val (d, rngD) = rb(rngI)

    f(i, d) -> rngD
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {

    def nextItem(randList: List[Rand[A]]): (List[A], RNG) = randList match {
      case Nil =>
        Nil -> rng
      case rand :: Nil =>
        val (i, nextRng) = rand(rng)
        (i :: Nil) -> nextRng
      case rand :: randTail =>
        val (list, currentRng) = nextItem(randTail)
        val (i, nextRng) = rand(currentRng)
        (list :+ i) -> nextRng
    }

    nextItem(fs)
  }

  def flatMap[A, B](f: Rand[A])
                   (g: A => Rand[B]): Rand[B] = rng => {
    val (v, nextRng) = f(rng)
    g(v)(nextRng)
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
