package pony
package brain
package jobs

object JobCounter {
  private var jobs = 0

  def next() = {
    jobs += 1
    jobs
  }
}
