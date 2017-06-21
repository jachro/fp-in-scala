package chapter7

import java.util.concurrent.ExecutorService

import scala.concurrent.Await.result
import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future}
import scala.util.{Success, Try}

trait Par[+A]

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A): Par[A] =
    (_: ExecutorService) => UnitFuture[A](a)

  private case class UnitFuture[A](get: A) extends Future[A] {

    def onComplete[U](f: Try[A] => U)
                     (implicit executor: ExecutionContext) = f(Success(get))

    def isCompleted = true

    def value = Some(Success(get))

    def ready(atMost: Duration)
             (implicit permit: CanAwait) = this

    def result(atMost: Duration)
              (implicit permit: CanAwait) = get
  }

  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) =>
      Future(result(a(es), Duration.Inf))(ExecutionContext.fromExecutor(es))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A, B, C](l: Par[A], r: Par[B])
                   (f: (A, B) => C): Par[C] = (es: ExecutorService) => {

    implicit val ec = ExecutionContext.fromExecutor(es)

    for {
      leftResult <- l(es)
      rightResult <- r(es)
    } yield f(leftResult, rightResult)
  }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = ???
}
