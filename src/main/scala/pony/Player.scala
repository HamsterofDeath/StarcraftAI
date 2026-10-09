package pony

import pony.brain.{HasUniverse, Universe}

case class Player(base: bwapi.Player)(override val universe: Universe) extends HasUniverse {
  val nativeRace = base.getRace
  val scRace     = SCRace.fromNative(nativeRace)
}
