package pony

import pony.brain.{HasUniverse, Universe}

import scala.jdk.CollectionConverters._

case class Player(base: bwapi.Player)(override val universe: Universe) extends HasUniverse {
  @volatile private var resolved       = Option.empty[SCRace]
  @volatile private var unitsCheckedAt = -1

  /**
    * The race, once it shows. BWAPI reports a Random opponent's race as Unknown even after its units were seen (still at
    * frame 37016 of the game won on ICCup Lost_Temple at 039677e), so until it reports one the race is the one most of
    * the player's visible units belong to, looked at once a frame.
    */
  def knownRace: Option[SCRace] = {
    if (resolved.isEmpty) {
      val reported = SCRace.known(base.getRace)
      resolved = reported.orElse(raceOfVisibleUnits)
      resolved.foreach { r =>
        val source = if (reported.isDefined) "game" else "units"
        NativeMatchEvidence.trace("race-known", s"player=${base.getName} race=$r source=$source")
      }
    }
    resolved
  }

  private def raceOfVisibleUnits =
    if (unitsCheckedAt == currentTick) None
    else {
      unitsCheckedAt = currentTick
      SCRace.mostOf(base.getUnits.asScala.map(_.getType.getRace))
    }

  /** The race of a player whose race is known; our own always is. */
  def scRace = knownRace.getOrElse(throw new IllegalStateException(s"The race of ${base.getName} is not known yet"))
}
