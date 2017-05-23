package chapter4

sealed trait Either[+E, +A] {

  def map[B](f: (A) => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case l@Left(_) => l
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case l@Left(_) => l
  }

  def orElse[EE >: E, B >: A](f: => Either[EE, B]): Either[EE, B] = this match {
    case r@Right(_) => r
    case _ => f
  }

  def map2[EE >: E, B, C](that: Either[EE, B])
                         (f: (A, B) => C): Either[EE, C] = for {
    thisRight <- this
    thatRight <- that
  } yield f(thisRight, thatRight)

  def map2Nel[EE >: E, EEE >: EE, B, C](that: Either[EE, B])
                                       (f: (A, B) => C): Either[List[EEE], C] = {
    this -> that match {
      case (Right(thisV), Right(thatV)) => Right(f(thisV, thatV))
      case _ => Left(
        Seq(this, that).foldLeft(List.empty[EEE]) {
          case (errors, Left(err)) => errors :+ err
          case (errors, _) => errors
        }
      )
    }
  }
}

case class Left[+E](error: E) extends Either[E, Nothing]

case class Right[+A](v: A) extends Either[Nothing, A]
