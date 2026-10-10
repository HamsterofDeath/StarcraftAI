package pony.e2e.chk

import java.nio.{ByteBuffer, ByteOrder}

/** A pre-placed unit of a UNIT section; `x`/`y` are its centre in pixels and `owner` a 0-based player. */
final case class MapUnit(unitId: Int, owner: Int, x: Int, y: Int) {
  def bytes(serial: Int): Array[Byte] = {
    val out = ByteBuffer.allocate(MapUnit.Size).order(ByteOrder.LITTLE_ENDIAN)
    out.putInt(serial).putShort(x.toShort).putShort(y.toShort).putShort(unitId.toShort)
    out.putShort(0.toShort) // relation
    out.putShort(0.toShort) // valid state flags
    out.putShort(MapUnit.ValidProperties.toShort)
    out.put(owner.toByte).put(100.toByte).put(100.toByte).put(100.toByte)
    out.array()
  }
}

object MapUnit {
  val Size = 36

  /** Owner, hit points, shields and energy are given. */
  private val ValidProperties = 0x0f

  val Marine        = 0
  val Ghost         = 1
  val Vulture       = 2
  val Goliath       = 3
  val SiegeTank     = 5
  val Scv           = 7
  val Wraith        = 8
  val ScienceVessel = 9
  val Battlecruiser = 12
  val Firebat       = 32
  val Medic         = 34
  val Zergling      = 37
  val Zealot        = 65
  val Dragoon       = 66
  val Corsair       = 60
  val HighTemplar   = 67
  val Archon        = 68
  val Scout         = 70
  val Reaver        = 83
  val Pylon         = 156
  val PhotonCannon  = 162
  val StartLocation = 214

  def atTile(unitId: Int, owner: Int, tileX: Int, tileY: Int) = MapUnit(unitId, owner, tileX * 32 + 16, tileY * 32 + 16)
}
