package chapter6

case class State[S, +A](run: S => (A, S)) {

  def unit[B](b: B): S => (B, S) =
    s => b -> s

  def map[B](f: A => B): S => (B, S) =
    s => {
      val (b, newS) = run(s)
      f(b) -> newS
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
}
