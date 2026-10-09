package pony
package brain
package modules

case class IdealUnitRatio[T <: Mobile](unitType: Class[_ <: Mobile], amount: Int)
                                      (active: => Boolean) {
  def fixedAmount = amount max 1

  def isActive = active
}
