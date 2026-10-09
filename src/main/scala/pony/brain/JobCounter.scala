package pony
package brain

object JobCounter {
  private var jobs = 0

  def next() = {
    jobs += 1
    jobs
  }
}
