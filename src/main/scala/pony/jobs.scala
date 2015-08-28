package pony

case class Priority(importance: Double) {

  def <(priority: Priority) = importance < priority.importance
  def >(priority: Priority) = importance > priority.importance
  def <=(priority: Priority) = importance <= priority.importance
}

object Priority {
  val None              = Priority(0)
  val DefaultBehaviour  = Priority(0.48)
  val Default           = Priority(0.49)
  val CollectMinerals   = Priority(0.5)
  val CollectGas        = Priority(0.55)
  val ConstructUnit     = Priority(0.64)
  val ConstructBuilding = Priority(0.63)
  val Upgrades          = Priority(0.65)
  val Addon             = Priority(0.66)
  val Supply            = Priority(0.7)
  val Max               = Priority(1)

  implicit val ordering = Ordering.by[Priority, Double](_.importance)
}