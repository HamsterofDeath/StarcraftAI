package pony

/** A place worth watching; a higher score wins, and the reason names what happens there. */
final case class CameraFocus(tile: MapTilePosition, score: Int, reason: String)

object CameraFocus {
  val CombatScore = 1000
  val EnemySightingScore = 500
  val ArmyScore = 100
  val HomeScore = 1
}
