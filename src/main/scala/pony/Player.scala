package pony

import pony.brain.{HasUniverse, Universe}

case class Player(base: bwapi.Player)(override val universe: Universe) extends HasUniverse {
  @volatile private var resolved = Option.empty[SCRace]

  /**
    * The race, once the game shows it: a Random opponent's race reads Unknown until one of its units has been seen, so
    * it is asked again until then.
    */
  def knownRace: Option[SCRace] = {
    if (resolved.isEmpty) {
      resolved = SCRace.known(base.getRace)
      resolved.foreach(r => NativeMatchEvidence.trace("race-known", s"player=${base.getName} race=$r"))
    }
    resolved
  }

  /** The race of a player whose race is known; our own always is. */
  def scRace = knownRace.getOrElse(throw new IllegalStateException(s"The race of ${base.getName} is not known yet"))
}
