package pony
package brain
package modules
package cruisers

/** Which enemy deserves a Yamato shot, kept free of the game. */
private[pony] object YamatoChoice {

  /** `share`: how much of the explosive Yamato damage the target's size lets through. */
  final case class Candidate(id: Int, value: Int, durability: Int, share: Double, hitsAir: Boolean, caster: Boolean)

  val Damage      = 260
  val Energy      = 150
  val RangePixels = 10 * 32

  /** A cast takes a few seconds: another cruiser leaves the target alone this long. */
  val ClaimFrames = 72

  /** Below this a shot is not worth its 150 energy. */
  val MinScore = 150.0

  val CasterBonus = 300

  def score(c: Candidate): Double = {
    val dealt = Damage * c.share
    val kill  = math.min(1.0, dealt / math.max(1, c.durability))
    val worth = c.value + (if (c.caster) CasterBonus else 0)
    worth * kill * (if (c.hitsAir || c.caster) 1.0 else 0.3)
  }

  def best(candidates: Seq[Candidate]): Option[Candidate] =
    candidates.map(c => c -> score(c)).filter(_._2 >= MinScore).maxByOption(_._2).map(_._1)
}
