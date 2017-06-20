package chapter7

trait Par[+A]

object Par {

  def unit[A](a: => A): Par[A] = ???

  def get[A](a: Par[A]): A = ???

  def map2[A, B, C](r: Par[A], l: Par[B])(f: (A, B) => C): Par[C] = ???
}
