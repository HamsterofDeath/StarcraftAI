package pony
package brain
package modules

/** Which Marines leave a bunker to stim, kept free of the game. */
private[pony] object BunkerStim {

  /** A Marine inside: its hit points and how long its stim lasts still (0: run out). */
  final case class Inside(id: Int, hitPoints: Int, stimTimer: Int)

  /** Stim costs ten hit points; below this a Marine stays in. */
  val MinHitPoints = 30

  /** Enemies this close make a bunker threatened; this close it is already fighting. */
  val ThreatTiles = 10
  val ReachTiles  = 6

  /** One unload per bunker this often at most. */
  val UnloadEvery = 24

  def toUnload(inside: Seq[Inside], fighting: Boolean, someoneOutside: Boolean): Seq[Int] = {
    val ready = inside.filter(m => m.stimTimer == 0 && m.hitPoints >= MinHitPoints).map(_.id)
    if (!fighting) ready
    else if (someoneOutside) Nil
    else ready.take(1)
  }

  /** A Marine on its way back in stims first while its bunker is threatened. */
  def stimsOnTheWay(threatened: Boolean, stimTimer: Int, hitPoints: Int) =
    threatened && stimTimer == 0 && hitPoints >= MinHitPoints
}
