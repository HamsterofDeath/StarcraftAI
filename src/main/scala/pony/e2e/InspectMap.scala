package pony.e2e

import pony.e2e.chk.ChkFile
import pony.e2e.mpq.MpqArchive

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Paths

/** Prints the scenario sections of a map: `sbt "runMain pony.e2e.InspectMap <map.scx>"`. */
object InspectMap {
  def main(args: Array[String]): Unit = {
    val chk = ChkFile.parse(MpqArchive.open(Paths.get(args(0))).read(ScenarioPath))
    chk.sections.foreach { case (name, data) => println(f"$name%-4s ${data.length}%7d bytes") }
    val dim = ByteBuffer.wrap(chk("DIM ")).order(ByteOrder.LITTLE_ENDIAN)
    val era = ByteBuffer.wrap(chk("ERA ")).order(ByteOrder.LITTLE_ENDIAN).getShort(0)
    println(s"size ${dim.getShort(0)}x${dim.getShort(2)} tileset $era")
    val tiles = ByteBuffer.wrap(chk("MTXM")).order(ByteOrder.LITTLE_ENDIAN).asShortBuffer()
    val counts = (0 until tiles.limit()).groupBy(i => tiles.get(i) & 0xffff).view.mapValues(_.size).toVector
    println("most common tiles: " + counts.sortBy(-_._2).take(8).map { case (t, n) => f"0x$t%04x:$n" }.mkString(" "))
  }

  val ScenarioPath = "staredit\\scenario.chk"
}
