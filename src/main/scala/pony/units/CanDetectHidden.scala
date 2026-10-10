package pony
package units

trait CanDetectHidden extends WrapsUnit with Detector {
  private val sight            = math.round(nativeUnitType.sightRange() / 32.0).toInt
  override def detectionRadius = sight
}
