package pony

import java.util.concurrent.locks.ReentrantReadWriteLock

class FutureIterator[IN, T](feed: => IN, produce: IN => T, startNow: Boolean) {
  private var nthHint                = 1
  private var name                   = "No name"
  private val lock                   = new ReentrantReadWriteLock()
  private var lastFeed               = Option.empty[IN]
  private var done                   = Option.empty[T]
  private var inProgress             = if (startNow) nextFuture else BWFuture.none
  private var thinking               = startNow
  private var calledForCurrentResult = false

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
    if (!thinking) {
      thinking = true
      lastFeed = None
      inProgress = nextFuture
    }
    lock.writeLock().unlock()
  }

  private def nextFuture = {
    val start = System.currentTimeMillis()
    val input = feed
    val fut   = BWFuture.produceFrom(produce(input))
    fut.future.foreach {
      case any =>
        lock.writeLock().lock()
        thinking = false
        done = any
        lastFeed = Some(input)
        calledForCurrentResult = false
        val duration = System.currentTimeMillis() - start
        debug(s"Future $name took $duration ms", duration > 0)
        lock.writeLock().unlock()
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
