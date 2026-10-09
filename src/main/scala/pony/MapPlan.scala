package pony

/**
  * The maps of a warm session: `-Dtwailight.mapPlan=maps/e2e/a.scm,maps/e2e/b.scm` plays them in order in one
  * StarCraft process, each game naming the next map before BWAPI restarts. Without a plan the bot plays one game.
  */
object MapPlan {
  val maps: Vector[String] =
    sys.props.get("twailight.mapPlan").toVector.flatMap(_.split(',').map(_.trim).filter(_.nonEmpty))

  def isSession: Boolean = maps.size > 1

  /** The map of the game after game `number` (1-based), if the plan has one. */
  def after(number: Int): Option[String] = maps.lift(number)

  def isLast(number: Int): Boolean = number >= maps.size
}
