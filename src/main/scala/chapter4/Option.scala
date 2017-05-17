package chapter4

sealed trait Option[+A] {

  def map[B](f: (A) => B): Option[B] =
    if (this == None) None
    else {
      val Some(v) = this
      Some(f(v))
    }

  def flatMap[B](f: (A) => Option[B]): Option[B] =
    if (this == None) None
    else {
      val Some(v) = this
      f(v)
    }

  def getOrElse[B >: A](default: => B): B =
    if (this == None) default
    else {
      val Some(v) = this
      v
    }

  def orElse[B >: A](default: => Option[B]): Option[B] =
    if (this == None) default
    else this

  def filter(predicate: (A) => Boolean): Option[A] =
    if (this == None) None
    else {
      val Some(v) = this

      if (predicate(v)) this
      else None
    }
}

case class Some[+A](a: A) extends Option[A]

case object None extends Option[Nothing]
