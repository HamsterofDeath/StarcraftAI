package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.e2e.GenerateE2EMaps
import pony.e2e.chk.{ChkFile, MapUnit, Trigger, UmsScenario}
import pony.e2e.mpq.{Explode, MpqArchive, MpqCrypto, MpqWriter}

import java.nio.{ByteBuffer, ByteOrder}

class E2EMapToolsTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Explode decodes the blast.c reference stream $explodeReference
       |Storm encryption round-trips $cryptRoundTrip
       |A written archive reads back its files $mpqRoundTrip
       |CHK sections round-trip through bytes $chkRoundTrip
       |A trigger occupies 2400 bytes and runs for its players $triggerLayout
       |The 4:6 vulture micro scenario builds a complete UMS scenario $kiteScenario
       |Scenario titles must be short and usable in a replay file name $titles
       |Map file names fit StarCraft's 31 characters even with a session prefix $fileNames
       |Cannon scenarios place powered, non-overlapping cannons with BWAPI's unit ids $cannonScenario
       """.stripMargin

  private def bytes(values: Int*) = values.map(_.toByte).toArray

  def explodeReference =
    new String(Explode(bytes(0x00, 0x04, 0x82, 0x24, 0x25, 0x8f, 0x80, 0x7f)), "ASCII") === "AIAIAIAIAIAIA"

  def cryptRoundTrip = {
    val data      = (0 until 64).map(_.toByte).toArray
    val key       = MpqCrypto.hash("(hash table)", MpqCrypto.FileKey)
    val encrypted = MpqCrypto.encrypt(data, key)
    (encrypted.toSeq must not(beEqualTo(data.toSeq))) and (MpqCrypto.decrypt(encrypted, key).toSeq === data.toSeq)
  }

  def mpqRoundTrip = {
    val a       = "first file".getBytes("ASCII")
    val b       = (0 until 5000).map(i => (i % 251).toByte).toArray
    val archive = new MpqArchive(MpqWriter.write(Seq("staredit\\scenario.chk" -> a, "other\\data.bin" -> b)))
    (archive.read("staredit\\scenario.chk").toSeq === a.toSeq) and
      (archive.read("other\\data.bin").toSeq === b.toSeq) and
      (archive.contains("missing.txt") must beFalse)
  }

  def chkRoundTrip = {
    val chk    = ChkFile(Vector("VER " -> bytes(205, 0), "DIM " -> bytes(64, 0, 64, 0), "MBRF" -> Array.emptyByteArray))
    val parsed = ChkFile.parse(chk.bytes)
    (parsed.names === chk.names) and (parsed("DIM ").toSeq === chk("DIM ").toSeq)
  }

  def triggerLayout = {
    val raw    = Trigger(Seq(0, 3), Seq(Trigger.always), Seq(Trigger.victory)).bytes
    val buffer = ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN)
    (raw.length === Trigger.Size) and
      (raw(15) === 22.toByte) and
      (raw(320 + 26) === 1.toByte) and
      (raw(2372) === 1.toByte) and
      (raw(2375) === 1.toByte) and
      (raw(2373) === 0.toByte) and
      (buffer.getInt(320 + 32) === 0)
  }

  def titles = {
    def build(title: String) =
      scala.util.Try(GenerateE2EMaps.micro(GenerateE2EMaps.Vulture, 8, GenerateE2EMaps.Zealot, 12).copy(name = title))
    (build("e2e micro: 8 vulture vs 12 zealot").isFailure must beTrue) and
      (GenerateE2EMaps.All.values.forall(s => s.name.length <= 31 && !s.name.contains(':')) must beTrue)
  }

  def fileNames = (GenerateE2EMaps.All.keys.forall(n => ("001-" + n + ".scx").length <= 31) must beTrue) and
    (GenerateE2EMaps.fileName(GenerateE2EMaps.Vulture, 8, GenerateE2EMaps.Zealot, 12) === "vulture8-zealot12")

  def cannonScenario = {
    val units   = GenerateE2EMaps.cannons(GenerateE2EMaps.Wraith, 4, 3).units
    val pylon   = units.filter(_.unitId == MapUnit.Pylon)
    val cannons = units.filter(_.unitId == MapUnit.PhotonCannon)
    // a pylon powers 8 tiles to each side and 5 tiles up and down
    def powered(c: MapUnit) = pylon.exists(p => math.abs(c.x - p.x) < 8 * 32 && math.abs(c.y - p.y) < 5 * 32)
    (MapUnit.Pylon === bwapi.UnitType.Protoss_Pylon.id) and
      (MapUnit.PhotonCannon === bwapi.UnitType.Protoss_Photon_Cannon.id) and
      (cannons.size === 3) and
      (cannons.forall(powered) must beTrue) and
      (cannons.combinations(2).forall { case Seq(a, b) => math.abs(a.y - b.y) >= 64 || math.abs(a.x - b.x) >= 64 }
        must beTrue) and
      (cannons.forall(c => c.x % 32 == 0 && c.y % 32 == 0) must beTrue)
  }

  def kiteScenario = {
    val template = ChkFile(Vector(
      "TYPE",
      "VER ",
      "IVE2",
      "VCOD",
      "PUNI",
      "UPGR",
      "PTEC",
      "UNIS",
      "UPGS",
      "TECS",
      "COLR",
      "PUPx",
      "PTEx",
      "UNIx",
      "UPGx",
      "TECx"
    ).map(_ -> bytes(1, 2)) :+ ("ERA " -> bytes(112, 0)))
    val chk =
      ChkFile.parse(GenerateE2EMaps.micro(GenerateE2EMaps.Vulture, 4, GenerateE2EMaps.Zealot, 6).build(template).bytes)
    val era = ByteBuffer.wrap(chk("ERA ")).order(ByteOrder.LITTLE_ENDIAN).getShort(0)
    (chk("MTXM").length === 64 * 64 * 2) and
      (chk("UNIT").length === 12 * 36) and
      (chk("TRIG").length === 4 * Trigger.Size) and
      (chk("MRGN").length === 255 * 20) and
      (era === 0.toShort) and
      (chk("SIDE")(0) === UmsScenario.Terran.toByte) and
      (chk("SIDE")(1) === UmsScenario.Protoss.toByte)
  }
}
