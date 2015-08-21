package pony

case class Priority(importance: Double) {

  def <(priority: Priority) = importance < priority.importance
  def <=(priority: Priority) = importance <= priority.importance
}
object Priority {
  val None              = Priority(0)
  val Default           = Priority(0.5)
  val CollectMinerals   = Priority(0.5)
  val CollectGas        = Priority(0.55)
  val Supply            = Priority(0.7)
  val Addon             = Priority(0.6)
  val ConstructBuilding = Priority(0.6)
  val ConstructUnit     = Priority(0.6)
  val Max               = Priority(1)
}