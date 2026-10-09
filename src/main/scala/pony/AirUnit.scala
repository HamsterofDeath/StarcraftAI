package pony

trait AirUnit extends Killable with Mobile {
  override def isGroundUnit = false
}
