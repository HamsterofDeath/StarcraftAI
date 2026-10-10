package pony
package units

trait AirUnit extends Killable with Mobile {
  override def isGroundUnit = false
}
