package pony
package brain

sealed trait Behaviour

case object AggressiveMove extends Behaviour

case object HoldPosition extends Behaviour

case object FallBack extends Behaviour

case object Undefined extends Behaviour
