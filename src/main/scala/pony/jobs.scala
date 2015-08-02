package pony

case class Priority(importance: Double)
object Priority {
  val None = Priority(0)
  val Max  = Priority(1)
}