package pony

object AttackPriorities {

  sealed case class AttackPriority(priority: Int) {
    def isHigh = priority > Medium.priority

    def isLow = priority < Medium.priority
  }

  object Highest extends AttackPriority(3)

  object Medium extends AttackPriority(2)

  object Lowest extends AttackPriority(1)

}
