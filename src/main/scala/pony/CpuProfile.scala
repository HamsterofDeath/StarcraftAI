package pony

import scala.collection.mutable

/**
  * Wall time per named section of the bot's frame, traced as "cpu-profile" once a game minute: the most expensive
  * sections with their average milliseconds per frame and their slowest single call.
  */
object CpuProfile {
  private val total   = mutable.HashMap.empty[String, Long]
  private val slowest = mutable.HashMap.empty[String, Long]
  private var frames  = 0
  private var started = System.nanoTime()

  /** Frames between two traces: one game minute. */
  val Window = 1440

  def time[A](section: String)(block: => A): A = {
    val start = System.nanoTime()
    try block
    finally {
      val spent = System.nanoTime() - start
      total(section) = total.getOrElse(section, 0L) + spent
      if (spent > slowest.getOrElse(section, 0L)) slowest(section) = spent
    }
  }

  def frameDone(): Unit = {
    frames += 1
    if (frames >= Window) {
      val wall = (System.nanoTime() - started) / 1e6 / frames
      NativeMatchEvidence.trace(
        "cpu-profile",
        f"wallMsPerFrame=$wall%.2f " + total.toVector.sortBy(-_._2).take(15).map { case (section, nanos) =>
          f"$section=${nanos / 1e6 / frames}%.2f/${slowest(section) / 1e6}%.0f"
        }.mkString(" ")
      )
      total.clear()
      slowest.clear()
      frames = 0
      started = System.nanoTime()
    }
  }
}
