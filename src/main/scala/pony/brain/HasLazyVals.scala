package pony
package brain

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait HasLazyVals extends IsTicked {
  private var counter = 0

  def uniqueKey = {
    val value = counter
    counter += 1
    Key(value)
  }

  case class Key(private val id: Int)

  private      val lazyVals              = ArrayBuffer.empty[LazyVal[_]]
  private lazy val lockedGroupedLazyVals = mutable.HashMap.empty[Key, ArrayBuffer[LazyVal[_]]]

  def explicitly[T](key: Key, t: => T, synchronize: Boolean = false) = {
    val newLazyVal = {
      if (synchronize) {
        SynchronizedLazyVal.from(t)
      } else {
        LazyVal.from(t)
      }
    }
    lockedGroupedLazyVals.getOrElseUpdate(key, ArrayBuffer.empty) += newLazyVal
    newLazyVal
  }

  def invalidate(key: Key) = {
    lockedGroupedLazyVals.get(key).foreach(_.foreach(_.invalidate()))
  }

  def oncePer[T](prime: PrimeNumber)(t: => T) = {
    var store: T = null.asInstanceOf[T]
    oncePerTick {
      if (store == null || currentTick % prime.i == 0) {
        store = t
      }
      store
    }
  }

  def oncePerTick[T](t: => T) = {
    val l = LazyVal.from(t)
    lazyVals += l
    l
  }

  def once[T](t: => T) = {
    SynchronizedLazyVal.from(t)
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    lazyVals.foreach(_.invalidate())
  }
}
