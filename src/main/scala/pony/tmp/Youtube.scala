package pony.tmp

import java.security.SecureRandom
import java.util.concurrent.atomic.AtomicLong

import scala.util.Random

/**
  * Created by HamsterofDeath on 4/9/2016.
  */
object Youtube {
  def main(args: Array[String]) {
    val rndPool: ThreadLocal[Random] = ThreadLocal.withInitial(() => new SecureRandom)

    def play = {
      var current = 0
      val target = Integer.parseInt("111111", 2)
      val rnd = rndPool.get()
      var count = 0
      while (current != target) {
        val slot = rnd.nextInt(6)
        current = current ^ (1 << slot)
        count += 1
      }
      count.toLong
    }

    val games = 1000
    val sum = new AtomicLong
    val counter = new AtomicLong()
    (1 to games).par foreach { _ =>
      1 to games foreach { _ =>
        1 to games foreach { _ =>
          val howMany = counter.incrementAndGet()
          sum.addAndGet(play)
          if (howMany % 1000000 == 0) {
            println(s"After $howMany steps the result is ${sum.get().toDouble / howMany}")
          }
        }
      }
    }

  }
}
