package pony
package units

trait AutoPilot extends Mobile {
  def isManuallyControlled  = !isAutoPilot
  override def isAutoPilot  = true
  override def isNonFighter = true
}
