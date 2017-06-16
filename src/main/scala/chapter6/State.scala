package chapter6

case class State[S, +A](run: S => (A, S)) {

  def unit[B](b: B): S => (B, S) =
    s => b -> s

  def map[B](f: A => B): S => (B, S) =
    s => {
      val (a, newS) = run(s)
      f(a) -> newS
    }

  def flatMap[B](f: A => (S => (B, S))): S => (B, S) =
    s => {
      val (a, newS) = run(s)
      f(a)(newS)
    }
}

object State {

  type State[S, +A] = S => (A, S)

  def map2[A, B, C, S](sa: S => (A, S), sb: S => (B, S))
                      (f: (A, B) => C): S => (C, S) =
    s => {
      val (a, nextS) = sa(s)
      val (b, nextNextS) = sb(nextS)
      f(a, b) -> nextNextS
    }

  def sequence[A, S](fs: List[S => (A, S)]): S => (List[A], S) =
    s => fs.foldLeft(List.empty[A] -> s) {
      case ((res, ss), f) =>
        val (v, nextS) = f(ss)
        (res :+ v) -> nextS
    }
}
