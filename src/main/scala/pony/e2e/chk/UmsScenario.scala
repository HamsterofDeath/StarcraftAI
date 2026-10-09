package pony.e2e.chk

import java.nio.{ByteBuffer, ByteOrder}

/**
  * A "use map settings" scenario on flat ground for end-to-end bot tests. Player 0 is the bot (a human slot), player 1
  * the computer. Verification data and unit/upgrade/tech defaults are copied from a template map; the terrain repeats
  * `groundTiles`, which must be walkable tiles of the template's tileset.
  */
final case class UmsScenario(name: String, description: String, widthTiles: Int, heightTiles: Int,
                             botRace: Int, opponentRace: Int, units: Seq[MapUnit], locations: Seq[Location],
                             triggers: Seq[Trigger], groundTiles: Seq[Int]) {

  def build(template: ChkFile): ChkFile = {
    val strings = Vector(name, description, "Bot", "Opponent", "Anywhere") ++ locations.map(_.name)
    def stringId(s: String) = strings.indexOf(s) + 1
    val copied = Vector("TYPE", "VER ", "IVE2", "VCOD", "PUNI", "UPGR", "PTEC", "UNIS", "UPGS", "TECS", "COLR", "PUPx",
      "PTEx", "UNIx", "UPGx", "TECx").map(n => n -> template(n))
    val tiles = mtxm
    ChkFile(copied ++ Vector(
      "OWNR" -> owners,
      "IOWN" -> owners,
      "ERA " -> le(2)(_.putShort((ByteBuffer.wrap(template("ERA ")).order(ByteOrder.LITTLE_ENDIAN).getShort(0) & 7)
                                 .toShort)),
      "DIM " -> le(4)(_.putShort(widthTiles.toShort).putShort(heightTiles.toShort)),
      "SIDE" -> sides,
      "MTXM" -> tiles,
      "TILE" -> tiles,
      "UNIT" -> units.zipWithIndex.flatMap { case (u, i) => u.bytes(i + 1) }.toArray,
      "THG2" -> Array.emptyByteArray,
      "DD2 " -> Array.emptyByteArray,
      "MASK" -> Array.fill(widthTiles * heightTiles)(0xff.toByte),
      "STR " -> stringTable(strings),
      "UPRP" -> new Array[Byte](64 * 20),
      "UPUS" -> new Array[Byte](64),
      "MRGN" -> regions(stringId("Anywhere"), stringId),
      "TRIG" -> triggers.flatMap(_.bytes).toArray,
      "MBRF" -> Array.emptyByteArray,
      "SPRP" -> le(4)(_.putShort(stringId(name).toShort).putShort(stringId(description).toShort)),
      "FORC" -> le(20) { b =>
        b.put(0.toByte).put(1.toByte).put(Array.fill(6)(0.toByte))
        b.putShort(stringId("Bot").toShort).putShort(stringId("Opponent").toShort).putShort(0.toShort).putShort(0.toShort)
        b.put(Array.fill(4)(0.toByte))
      },
      "WAV " -> new Array[Byte](512 * 4),
      "SWNM" -> new Array[Byte](256 * 4)
    ))
  }

  private def owners = {
    val o = Array.fill[Byte](12)(0)
    o(0) = UmsScenario.Human
    o(1) = UmsScenario.Computer
    o
  }

  private def sides = {
    val s = Array.fill[Byte](12)(UmsScenario.InactiveRace)
    s(0) = botRace.toByte
    s(1) = opponentRace.toByte
    s(11) = UmsScenario.NeutralRace
    s
  }

  /** Mixes the ground tiles in a fixed pattern so the map looks natural and stays deterministic. */
  private def mtxm = le(widthTiles * heightTiles * 2) { b =>
    (0 until widthTiles * heightTiles).foreach(i => b.putShort(groundTiles((i * 7 + i / widthTiles) % groundTiles.size)
                                                               .toShort))
  }

  private def regions(anywhereName: Int, stringId: String => Int) = le(255 * 20) { b =>
    def put(number: Int, left: Int, top: Int, right: Int, bottom: Int, nameId: Int) = {
      b.position((number - 1) * 20)
      b.putInt(left).putInt(top).putInt(right).putInt(bottom).putShort(nameId.toShort).putShort(0.toShort)
    }
    put(Trigger.Anywhere, 0, 0, widthTiles * 32, heightTiles * 32, anywhereName)
    locations.foreach(l => put(l.number, l.left, l.top, l.right, l.bottom, stringId(l.name)))
  }

  private def stringTable(strings: Vector[String]) = {
    val encoded = strings.map(_.getBytes("ISO-8859-1") :+ 0.toByte)
    val headerSize = 2 + strings.size * 2
    le(headerSize + encoded.map(_.length).sum) { b =>
      b.putShort(strings.size.toShort)
      encoded.scanLeft(headerSize)(_ + _.length).init.foreach(offset => b.putShort(offset.toShort))
      encoded.foreach(b.put)
    }
  }

  private def le(size: Int)(fill: ByteBuffer => Unit): Array[Byte] = {
    val b = ByteBuffer.allocate(size).order(ByteOrder.LITTLE_ENDIAN)
    fill(b)
    b.array()
  }
}

object UmsScenario {
  val Zerg    = 0
  val Terran  = 1
  val Protoss = 2

  private val Human        = 6.toByte
  private val Computer     = 5.toByte
  private val NeutralRace  = 4.toByte
  private val InactiveRace = 7.toByte
}
