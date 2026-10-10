package pony
package brain
package modules
package production

import pony.units.Mobile

case class IdealUnitRatio[T <: Mobile](unitType: Class[? <: Mobile], amount: Int)(active: => Boolean) {
  def fixedAmount = amount max 1

  def isActive = active
}
