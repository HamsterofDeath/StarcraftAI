package pony

import bwapi.PlayerType
import pony.brain.{HasUniverse, Universe}

class Forces(
    me: bwapi.Player,
    myAllies: Set[bwapi.Player],
    myEnemies: Set[bwapi.Player],
    override val universe: Universe
) extends HasUniverse {

  def myRace = myself.scRace

  def isTvT = {
    forces.myself.scRace.isTerran &&
    forces.hostilePlayers.size == 1 &&
    forces.hostilePlayers.players.head.scRace.isTerran
  }

  def isTvZ = {
    forces.myself.scRace.isTerran &&
    forces.hostilePlayers.size == 1 &&
    forces.hostilePlayers.players.head.scRace.isZerg
  }

  def isTvP = {
    forces.myself.scRace.isTerran &&
    forces.hostilePlayers.size == 1 &&
    forces.hostilePlayers.players.head.scRace.isProtoss
  }

  val myself         = Player(me)(universe)
  val alliedPlayers  = Force(myAllies.map(Player(_)(universe)))(universe)
  val hostilePlayers = Force(myEnemies.map(Player(_)(universe)))(universe)

  def isNotEnemy(u: bwapi.Unit) = !isEnemy(u)

  def isEnemy(u: bwapi.Unit) = !isNeutral(u) && !isFriend(u)

  def isFriend(u: bwapi.Unit) = {
    u.getPlayer == me || myAllies(u.getPlayer)
  }

  def isNeutral(u: bwapi.Unit) = u.getPlayer.getType == PlayerType.None
}
