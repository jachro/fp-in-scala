package chapter7

import java.util.concurrent.ExecutorService

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

  def fork[A](a: => Par[A]): Par[A] = es => {

    implicit val ec = ExecutionContext.fromExecutor(es)

    for {
      nestedFuture <- Future(a(es))
      future <- nestedFuture
    } yield future
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A, B, C](l: Par[A], r: Par[B])
                   (f: (A, B) => C): Par[C] = es => {

    implicit val ec = ExecutionContext.fromExecutor(es)

    for {
      leftResult <- l(es)
      rightResult <- r(es)
    } yield f(leftResult, rightResult)
  }

  def map[A, B](pa: Par[A])
               (f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])
                  (f: A => B): Par[List[B]] = fork {
    sequence(ps map asyncF(f))
  }

  def parFilter[A](as: List[A])
                  (f: A => Boolean): Par[List[A]] = fork {

    val ff: A => (Boolean, A) = a => f(a) -> a

    as.map(asyncF(ff)).foldLeft(unit(List.empty[A])) {
      case (res, ffRes) => map2(res, ffRes) {
        case (resList, (true, item)) => item :: resList
        case (resList, _) => resList
      }
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(unit(List.empty[A])) {
      case (res, listItem) => map2(res, listItem)((resList, item) => item :: resList)
    }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = ???
}
