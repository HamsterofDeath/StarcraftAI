package pony
package brain

case class Objective(target: Option[TargetPosition], how: Behaviour)

object Objective {
  val initial = Objective(None, Undefined)
}
