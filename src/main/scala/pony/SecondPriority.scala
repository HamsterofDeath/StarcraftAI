package pony

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

  implicit val ordering: Ordering[SecondPriority] = Ordering.by[SecondPriority, Double](_.importance)
}
