package pony

import pony.brain.{HasUniverse, Universe}

case class Force(players: Set[Player])(override val universe: Universe) extends HasUniverse {
  def size = players.size
}
