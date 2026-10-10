package pony
package util

import scala.collection.mutable

/** A mutable map from each key to a set of values, replacing the deprecated `mutable.MultiMap` mixin. */
final class MultiMap[K, V] extends mutable.AbstractMap[K, mutable.Set[V]] {
  private val underlying = mutable.HashMap.empty[K, mutable.Set[V]]

  def addBinding(key: K, value: V): this.type = {
    underlying.getOrElseUpdate(key, mutable.HashSet.empty[V]) += value
    this
  }

  /** Removes the value and drops the key once its set is empty, like the old mixin. */
  def removeBinding(key: K, value: V): this.type = {
    underlying.get(key).foreach { values =>
      values -= value
      if (values.isEmpty) underlying -= key
    }
    this
  }

  def toImmutable: Map[K, Set[V]] = underlying.iterator.map { case (k, v) => k -> v.toSet }.toMap

  override def get(key: K): Option[mutable.Set[V]] = underlying.get(key)

  override def iterator: Iterator[(K, mutable.Set[V])] = underlying.iterator

  override def addOne(elem: (K, mutable.Set[V])): this.type = {
    underlying.addOne(elem)
    this
  }

  override def subtractOne(key: K): this.type = {
    underlying.subtractOne(key)
    this
  }

  override def clear(): Unit = underlying.clear()

  override def size: Int = underlying.size

  override def knownSize: Int = underlying.knownSize
}
