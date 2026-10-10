package pony
package util

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

class BWFuture[+T](val future: Future[T], incomplete: T) {

  def blockAndGet = {
    Await.result(future, Duration.Inf)
  }

  def assumeDoneAndGet = {
    assert(future.isCompleted)
    result
  }

  def idle = isDone

  def isDone = future.isCompleted

  def map[X](f: T => X) = new BWFuture(future.map(f), f(incomplete))

  def ifDone[X](ifDone: T => X): Unit = {
    if (future.isCompleted) {
      ifDone(result)
    }
  }

  def result = future.value match {
    case Some(Success(x)) => x
    case Some(Failure(e)) => throw e
    case _                => incomplete
  }

  def matchOnSelf[X](ifDone: T => X, ifRunning: => X) = {
    if (future.isCompleted) {
      ifDone(result)
    } else {
      ifRunning
    }
  }

}

object BWFuture {

  def none[T] = apply(Option.empty[T])

  def apply[T](produce: => Option[T]): BWFuture[Option[T]] = BWFuture(produce, None)

  def apply[T](produce: => T, ifIncomplete: T) = {
    val fut = Future { produce }
    new BWFuture(fut, ifIncomplete)
  }

  def from[T](produce: => T): BWFuture[Option[T]] = {
    BWFuture(Some(produce), None)
  }

  def produceFrom[T](produce: => T) = BWFuture(Some(produce))

  implicit class Result[T](val fut: BWFuture[Option[T]]) extends AnyVal {
    def orElse(other: T) = if (fut.isDone) fut.result.get else other

    def imap[X](f: T => X) = fut.map(_.map(f))

    def ifDoneOpt[X](ifDone: T => X): Unit = {
      fut.ifDone(_.foreach(ifDone))
    }

    def foldOpt[X](ifRunning: => X)(ifDone: T => X) = {
      matchOnOptSelf(ifDone, ifRunning)
    }

    def matchOnOptSelf[X](ifDone: T => X, ifRunning: => X) = {
      fut.matchOnSelf(
        {
          case Some(t) => ifDone(t)
          case _       => ifRunning
        },
        ifRunning
      )
    }
  }

}
