package pony.e2e

import pony.e2e.chk.ChkFile
import pony.e2e.mpq.MpqArchive

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Paths

/**
  * Prints a map's terrain as text, one line per tile row, without starting the game: '.' walkable and buildable, ','
  * walkable only, '~' partly walkable, '#' not walkable, 'm' minerals, 'g' a geyser, 's' a start location, and digits
  * for the height (0 low, 1 middle, 2 high) when `heights` is given. `sbt "runMain pony.e2e.TraceTerrain <map.scx>
  * <StarDat.mpq> x0 y0 x1 y1 [heights]"`.
  */
object TraceTerrain {
  def main(args: Array[String]): Unit = {
    val chk                    = ChkFile.parse(MpqArchive.open(Paths.get(args(0))).read(InspectMap.ScenarioPath))
    val data                   = MpqArchive.open(Paths.get(args(1)))
    val Array(x0, y0, x1, y1)  = args.slice(2, 6).map(_.toInt)
    val heights                = args.lift(6).contains("heights")
    def le(bytes: Array[Byte]) = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    val dim                    = le(chk("DIM "))
    val width                  = dim.getShort(0) & 0xffff
    val era                    = (le(chk("ERA ")).getShort(0) & 0xffff) % 8
    val tileset = Vector("badlands", "platform", "install", "ashworld", "jungle", "desert", "ice", "twilight")(era)
    val cv5     = le(data.read(s"tileset\\$tileset.cv5"))
    val vf4     = le(data.read(s"tileset\\$tileset.vf4"))
    val mtxm    = le(chk.overlaid("MTXM"))
    def megatile(x: Int, y: Int) = {
      val id = mtxm.getShort((y * width + x) * 2) & 0xffff
      cv5.getShort((id >> 4) * 52 + 20 + (id & 0xf) * 2) & 0xffff
    }
    def groupFlags(x: Int, y: Int) = {
      val id = mtxm.getShort((y * width + x) * 2) & 0xffff
      cv5.getShort((id >> 4) * 52 + 2) & 0xffff
    }
    def mini(x: Int, y: Int) = (0 until 16).map(i => vf4.getShort(megatile(x, y) * 32 + i * 2) & 0xffff)
    val marks                = scala.collection.mutable.HashMap.empty[(Int, Int), Char]
    val units                = le(chk("UNIT"))
    (0 until chk("UNIT").length / 36).foreach { i =>
      val (px, py, kind) =
        (units.getShort(i * 36 + 4) & 0xffff, units.getShort(i * 36 + 6) & 0xffff, units.getShort(i * 36 + 8) & 0xffff)
      val (w, h, c) = kind match {
        case 176 | 177 | 178 => (2, 1, 'm')
        case 188             => (4, 2, 'g')
        case 214             => (4, 3, 's')
        case _               => (0, 0, ' ')
      }
      val (tx, ty) = ((px - w * 16) / 32, (py - h * 16) / 32)
      for (dx <- 0 until w; dy <- 0 until h) marks((tx + dx, ty + dy)) = c
    }
    if (args.lift(6).contains("mini")) {
      // one character per 8-pixel walk tile, a space between tiles
      println("      " + (x0 to x1).map(x => f"$x%-4d").mkString(" "))
      (y0 to y1).foreach { y =>
        (0 until 4).foreach { my =>
          val row = (x0 to x1).map { x =>
            marks.get((x, y)).map(c => c.toString * 4).getOrElse {
              val flags = mini(x, y)
              (0 until 4).map(mx => if ((flags(my * 4 + mx) & 0x1) != 0) '.' else '#').mkString
            }
          }.mkString(" ")
          println(f"y=$y%3d $row")
        }
      }
      return
    }
    println("      " + (x0 to x1).map(x => (x / 10 % 10).toString).mkString)
    println("      " + (x0 to x1).map(x => (x % 10).toString).mkString)
    (y0 to y1).foreach { y =>
      val row = (x0 to x1).map { x =>
        marks.getOrElse(
          (x, y), {
            val flags    = mini(x, y)
            val walkable = flags.count(f => (f & 0x1) != 0)
            if (walkable == 0) '#'
            else if (heights)
              (if (flags.exists(f => (f & 0x4) != 0)) '2' else if (flags.exists(f => (f & 0x2) != 0)) '1' else '0')
            else if (walkable < 16) '~'
            else if ((groupFlags(x, y) & 0x80) == 0) '.'
            else ','
          }
        )
      }.mkString
      println(f"y=$y%3d $row")
    }
  }
}
