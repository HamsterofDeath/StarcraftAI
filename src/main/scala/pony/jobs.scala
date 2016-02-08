package pony

case class Priority(importance: Double) {

  def <(priority: Priority) = importance < priority.importance

  def >(priority: Priority) = importance > priority.importance

  def <=(priority: Priority) = importance <= priority.importance
}

case class SecondPriority(importance: Double) {

  def <(priority: SecondPriority) = importance < priority.importance

  def >(priority: SecondPriority) = importance > priority.importance

  def <=(priority: SecondPriority) = importance <= priority.importance
}

object SecondPriority {
  val None              = SecondPriority(0)
  val BetterThanNothing = SecondPriority(0.05)
  val EvenLess          = SecondPriority(0.1)
  val Less              = SecondPriority(0.25)
  val Default           = SecondPriority(0.5)
  val More              = SecondPriority(0.75)
  val EvenMore          = SecondPriority(0.9)
  val Max               = SecondPriority(1.0)

  implicit val ordering = Ordering.by[SecondPriority, Double](_.importance)
}

object Priority {
  val None              = Priority(0)
  val DefaultBehaviour  = Priority(0.48)
  val Default           = Priority(0.49)
  val CollectMinerals   = Priority(0.5)
  val CollectGas        = Priority(0.55)
  val ConstructUnit     = Priority(0.64)
  val ConstructBuilding = Priority(0.63)
  val Addon             = Priority(0.66)
  val Supply            = Priority(0.7)
  val Upgrades          = Priority(0.71)
  val Expand            = Priority(0.72)
  val Max               = Priority(1)

  implicit val ordering = Ordering.by[Priority, Double](_.importance)
}