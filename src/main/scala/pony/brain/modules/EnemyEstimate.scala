package pony
package brain
package modules

/** The arithmetic of the enemy army bound, kept free of the game; values in minerals plus gas, times in frames. */
private[pony] object EnemyEstimate {

  /** A saturated base earns about this much a frame (some 900 minerals and 300 gas a game minute). */
  val DefaultRatePerBase = 0.8

  /** A base reaches its full income this long after it starts (workers have to be trained first). */
  val RampFrames = 24 * 60 * 6

  /** An expansion may have run this long before we saw it, but not before the third minute. */
  val ExpansionLead     = 24 * 60 * 3
  val EarliestExpansion = 24 * 60 * 3

  /** Workers a full income needs per base, and what they cost. */
  val WorkersPerBase = 19
  val WorkerCost     = 50

  val StartMinerals = 50
  val RateWindow    = 24 * 60
  val ReportFrames  = 24 * 60
  val RecentFrames  = 24 * 30
  val FarTiles      = 25

  final case class Estimate(bases: Int, gathered: Double, buildings: Double, workers: Double, lostArmy: Double) {
    def upper: Double = math.max(0.0, gathered + StartMinerals - buildings - workers - lostArmy)
  }

  final case class Model(now: Int, baseStarts: Seq[Int], ratePerBase: Double, buildings: Double, lostArmy: Double) {
    def estimate = Estimate(
      baseStarts.size,
      baseStarts.map(s => income(math.max(0, now - s), ratePerBase)).sum,
      buildings,
      workers,
      lostArmy
    )

    /** At least the four starting workers; a full income needs WorkersPerBase on every base. */
    def workers: Double = math.max(4, baseStarts.size * WorkersPerBase - 4).toDouble * WorkerCost
  }

  /** What a base earns in `frames`: rising from a fifth to the full rate over RampFrames, then steady. */
  def income(frames: Int, rate: Double): Double = {
    val ramp   = math.min(frames, RampFrames).toDouble
    val rising = ramp * rate * (0.2 + 0.4 * ramp / RampFrames)
    rising + math.max(0, frames - RampFrames) * rate
  }
}
