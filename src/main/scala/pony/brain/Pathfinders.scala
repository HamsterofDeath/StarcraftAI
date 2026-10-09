package pony
package brain

trait Pathfinders {
  def ground: PathFinder
  def groundSafe: PathFinder
  def airSafe: PathFinder
  def safeFor[T <: Mobile](m: T) = {
    m match {
      case g: GroundUnit if g.onGround => groundSafe
      case a: AirUnit                  => airSafe
      case _                           => !!!(s"Invalid request: $m")
    }
  }
}
