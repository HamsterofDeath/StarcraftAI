package pony
package brain
package modules

trait DefaultDangerAreaConfig[T <: Mobile] extends AvoidSpecificAreas[T] {
  override protected def tolerance             = 2
  override protected def targetReuseAllowance  = 8
  override protected def updateWhen            = Primes.prime5
  override protected val actionName: String    = "<!>"
  override protected def tilesToAvoidAsBlocked = mapLayers.underPsiStorm.toSome
}
