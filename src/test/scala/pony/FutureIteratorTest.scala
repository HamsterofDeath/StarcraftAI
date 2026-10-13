package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.util.FutureIterator

import java.util.concurrent.atomic.AtomicInteger

class FutureIteratorTest extends Specification with MustMatchers {

  def is =
    s2"""
       |An iterator whose first result came at once still calculates the next one $recalculates
       |An iterator whose calculation failed calculates the next one $recoversFromFailure
       """.stripMargin

  private def waitFor(condition: => Boolean) = {
    val until = System.currentTimeMillis() + 10000
    while (!condition && System.currentTimeMillis() < until) Thread.sleep(5)
    condition
  }

  // A trivial first result can come before the constructor is done; it must not leave the iterator thinking forever.
  def recalculates = {
    val generation = new AtomicInteger(0)
    val iterators  = Vector.fill(20000)(FutureIterator.feed(generation.get).produceAsync(identity))
    waitFor(iterators.forall(_.hasResult))
    generation.set(1)
    iterators.foreach(_.prepareNextIfDone())
    waitFor(iterators.forall(_.mostRecent.contains(1)))
    iterators.count(!_.mostRecent.contains(1)) === 0
  }

  def recoversFromFailure = {
    val generation = new AtomicInteger(0)
    val iterator   = FutureIterator.feed(generation.get).produceAsync { g =>
      if (g == 0) throw new IllegalStateException("first calculation fails") else g
    }
    Thread.sleep(50)
    generation.set(1)
    waitFor {
      iterator.prepareNextIfDone()
      iterator.mostRecent.contains(1)
    } must beTrue
  }
}
