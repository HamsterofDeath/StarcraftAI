package pony.e2e

import bwapi.{BWClient, DefaultBWListener}

import scala.jdk.CollectionConverters.*

/**
  * Plays a replay through BWAPI as fast as it goes and prints, every few frames, the units inside a tile rectangle:
  * where each stands, its order and where that order leads, and a map of the rectangle ('#' not walkable, a digit the
  * number of units on a tile, a letter a building). Shows what held an army up that the bot's own traces cannot.
  * `java -cp ... pony.e2e.ReplayDump x0,y0,x1,y1 from,to,every` while StarCraft plays the replay (bwapi.ini map = the
  * .rep file).
  */
object ReplayDump {
  def main(args: Array[String]): Unit = {
    val Array(x0, y0, x1, y1)  = args(0).split(',').map(_.toInt)
    val Array(from, to, every) = args(1).split(',').map(_.toInt)
    var client: BWClient       = null
    client = new BWClient(new DefaultBWListener {
      override def onStart(): Unit = {
        val g = client.getGame
        g.setLocalSpeed(0)
        g.setGUI(false)
        println(
          s"replay=${g.isReplay} map=${g.mapName} players=" +
            g.getPlayers.asScala.map(p => s"${p.getID}:${p.getName}:${p.getRace}").mkString(",")
        )
      }

      override def onFrame(): Unit = {
        val g = client.getGame
        val f = g.getFrameCount
        if (args.lift(2).contains("regions")) {
          regions(g)
          g.leaveGame()
        } else if (f > to) g.leaveGame()
        else if (f >= from && (f - from) % every == 0) dump(g, f)
      }

      override def onEnd(won: Boolean): Unit = {
        println("end")
        System.exit(0)
      }
    })

    /**
      * StarCraft's own pathing regions in the rectangle, one character per tile (by the region at the tile's centre,
      * '#' none), then each region's centre and neighbours: the native pathfinder routes units between these.
      */
    def regions(g: bwapi.Game): Unit = {
      val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
      val seen  = scala.collection.mutable.LinkedHashMap.empty[Int, bwapi.Region]
      def label(r: bwapi.Region) = chars(seen.keys.toVector.indexOf(r.getID) % chars.size)
      println("      " + (x0 to x1).map(x => (x / 10 % 10).toString).mkString)
      println("      " + (x0 to x1).map(x => (x % 10).toString).mkString)
      (y0 to y1).foreach { y =>
        val row = (x0 to x1).map { x =>
          Option(g.getRegionAt(x * 32 + 16, y * 32 + 16)).filter(_.isAccessible).map { r =>
            seen.getOrElseUpdate(r.getID, r)
            label(r)
          }.getOrElse('#')
        }.mkString
        println(f"y=$y%3d $row")
      }
      seen.values.foreach { r =>
        val c = r.getCenter.toTilePosition
        println(
          s"${label(r)} id=${r.getID} centre=$c group=${r.getRegionGroupID} higher=${r.isHigherGround} " +
            s"box=${r.getBoundsLeft / 32},${r.getBoundsTop / 32}-${r.getBoundsRight / 32},${r.getBoundsBottom / 32} " +
            s"neighbours=" + r.getNeighbors.asScala.filter(_.isAccessible).map { n =>
              s"${seen.get(n.getID).map(label).getOrElse('?')}${n.getID}@${n.getCenter.toTilePosition}"
            }.mkString(",")
        )
      }
    }

    def dump(g: bwapi.Game, f: Int): Unit = {
      def inside(t: bwapi.TilePosition) = t.x >= x0 && t.x <= x1 && t.y >= y0 && t.y <= y1
      val units = g.getAllUnits.asScala.filter(u => u.exists && inside(u.getPosition.toTilePosition)).toVector
        .sortBy(u => (u.getPosition.y, u.getPosition.x))
      val marks = scala.collection.mutable.HashMap.empty[(Int, Int), Char]
      units.foreach { u =>
        val kind = u.getType
        if (kind.isBuilding || kind.isMineralField || kind == bwapi.UnitType.Resource_Vespene_Geyser) {
          val t = u.getTilePosition
          val c = if (kind.isMineralField) 'm' else if (u.getPlayer == g.self()) kind.toString.split('_').last.head
          else kind.toString.split('_').last.head.toLower
          for (dx <- 0 until kind.tileWidth; dy <- 0 until kind.tileHeight) marks((t.x + dx, t.y + dy)) = c
        } else if (!u.isFlying && !u.isLoaded) {
          val t = u.getPosition.toTilePosition
          val n = marks.get((t.x, t.y)).filter(_.isDigit).map(_.asDigit).getOrElse(0) + 1
          marks((t.x, t.y)) = if (n > 9) '+' else ('0' + n).toChar
        }
      }
      println(s"frame=$f units=${units.size}")
      (y0 to y1).foreach { y =>
        val row = (x0 to x1).map { x =>
          marks.getOrElse(
            (x, y), {
              val walk = (for (wx <- 0 until 4; wy <- 0 until 4) yield g.isWalkable(x * 4 + wx, y * 4 + wy)).count(identity)
              if (walk == 0) '#' else if (walk < 16) '~' else '.'
            }
          )
        }.mkString
        println(f"  y=$y%3d $row")
      }
      units.filterNot(u => u.getType.isMineralField || u.getType.isBuilding).foreach { u =>
        val target = Option(u.getTarget).map(t => s" target=${t.getID}:${t.getType}").getOrElse("")
        println(
          s"  ${u.getID} ${u.getPlayer.getID} ${u.getType.toString.replaceAll("^[A-Za-z]+_", "")} " +
            f"at=${u.getPosition.x / 32.0}%.1f,${u.getPosition.y / 32.0}%.1f" +
            s" order=${u.getOrder} to=${u.getOrderTargetPosition.toTilePosition} move=${u.getTargetPosition.toTilePosition}" +
            s" hp=${u.getHitPoints}${if (u.isSieged) " sieged" else ""}${if (u.isHoldingPosition) " hold" else ""}" +
            s"${if (u.isIdle) " idle" else ""}${if (u.isStuck) " stuck" else ""}$target"
        )
      }
      units.filter(_.getType.isBuilding).foreach { u =>
        println(s"  ${u.getID} ${u.getPlayer.getID} ${u.getType} tile=${u.getTilePosition} hp=${u.getHitPoints}")
      }
    }

    client.startGame()
  }
}
