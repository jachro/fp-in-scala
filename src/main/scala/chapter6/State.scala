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
}
