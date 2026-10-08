package pony

/**
 * Heavy AI planning cadence: expensive calculations and command issuance run once per
 * game second (24 frames) by default. 1 restores the old every-frame behaviour.
 */
object AiCadence {
  val frames: Int = math.max(1, sys.props.getOrElse("twailight.aiTickFrames", "24").toInt)
  def heavyNow(tick: Int): Boolean = tick % frames == 0
}
