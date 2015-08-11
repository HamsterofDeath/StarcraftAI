package pony

case class Priority(importance: Double) {

  def <(priority: Priority) = importance < priority.importance
}
object Priority {
  val None              = Priority(0)
  val Default           = Priority(0.5)
  val Supply            = Priority(0.7)
  val ConstructBuilding = Priority(0.6)
  val ConstructUnit     = Priority(0.6)
  val Max               = Priority(1)
}