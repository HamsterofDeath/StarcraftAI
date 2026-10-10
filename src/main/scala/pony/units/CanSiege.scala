package pony
package units

trait CanSiege extends Mobile {
  private val sieged = oncePerTick {
    nativeUnit.isSieged
  }
  def isSieged = sieged.get
}
