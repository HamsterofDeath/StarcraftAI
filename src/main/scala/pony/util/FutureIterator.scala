package pony
package util

import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.util.{Failure, Success}

class FutureIterator[IN, T](feed: => IN, produce: IN => T, startNow: Boolean) {
  private var nthHint                = 1
  private var name                   = "No name"
  private val lock                   = new ReentrantReadWriteLock()
  private var lastFeed               = Option.empty[IN]
  private var done                   = Option.empty[T]
  private var calledForCurrentResult = false
  private var failures               = 0
  // Set before the first calculation starts. A quick one used to finish before the constructor set it, and the
  // iterator then thought forever and never calculated again: 742 of 20000 in FutureIteratorTest. In game 5 vs Protoss
  // on ICCup Lost_Temple at 87b3736 no raid was ever reported while twelve zealots killed our workers by the CC.
  private var thinking   = startNow
  private var inProgress = if (startNow) nextFuture else BWFuture.none

  def setupRecalcHint(tick: PrimeNumber) = {
    triggerRecalcOn(tick.i)
    this
  }

  def triggerRecalcOn(tick: Int) = tick % nthHint == 0

  def named(name: String) = {
    this.name = name
    this
  }

  def onMostRecent[X](f: T => X) = {
    mostRecent.foreach(f)
  }

  def onceIfDone[X](f: T => X) = {
    lock.readLock().lock()
    if (!calledForCurrentResult && hasResult) {
      calledForCurrentResult = true
      f(mostRecentAssumeCalculated)
    }
    lock.readLock().unlock()
  }

  def mapOnContent[X](f: T => X) = {
    mostRecent.map(f)
  }

  def flatMapOnContent[X](f: T => Option[X]) = {
    mostRecent.flatMap(f)
  }

  def hasResult = mostRecent.isDefined

  def mostRecent = {
    lock.readLock().lock()
    val x = done
    lock.readLock().unlock()
    x
  }

  def feedObj = feed

  def lastUsedFeed = {
    lock.readLock().lock()
    val x = lastFeed
    lock.readLock().unlock()
    x
  }

  def mostRecentAssumeCalculated = mostRecent.get

  def prepareNextIfDone(): Unit = {
    lock.writeLock().lock()
    try {
      if (!thinking) {
        // the calculation reports back under this lock, so it cannot finish before these are set
        inProgress = nextFuture
        thinking = true
        lastFeed = None
      }
    } finally lock.writeLock().unlock()
  }

  /** A failed calculation keeps the last result and lets the next one start; it used to leave the iterator thinking. */
  private def nextFuture = {
    val start = System.currentTimeMillis()
    val input = feed
    val fut   = BWFuture.produceFrom(produce(input))
    fut.future.onComplete { result =>
      lock.writeLock().lock()
      try {
        thinking = false
        result match {
          case Success(any) =>
            done = any
            lastFeed = Some(input)
            calledForCurrentResult = false
          case Failure(e) =>
            failures += 1
            if (failures == 1) NativeMatchEvidence.trace("future-iterator-failed", s"name=$name error=$e")
        }
        val duration = System.currentTimeMillis() - start
        debug(s"Future $name took $duration ms", duration > 0)
      } finally lock.writeLock().unlock()
    }
    fut
  }
}

object FutureIterator {
  class Feeder[IN](in: => IN) {
    def produceAsync[T](produce: IN => T) = {
      new FutureIterator(in, produce, true)
    }

    def produceAsyncLater[T](produce: IN => T) = {
      new FutureIterator(in, produce, false)
    }
  }

  def feed[IN](in: => IN) = new Feeder(in)
}
