package pony

import bwapi.Game

case class ResourceArea(patches: MineralPatchGroup, geysirs: Set[Geysir]) {
  val resources = patches.patches ++ geysirs
  def center = patches.center
}

case class PotentialDomain(coveredOnLand: Seq[ResourceArea], needsToControl: Seq[MapTilePosition])
case class ChokePoint(center: MapTilePosition, lines: Seq[CuttingLine], index: Int = -1)

case class RelativePoint(xOff: Int, yOff: Int) extends HasXY {
  lazy val opposite = RelativePoint(-xOff, -yOff)
  def asMapTile = MapTilePosition.shared(x, y)
  override def x = xOff
  override def y = yOff
}

case class CuttingLine(line: Line) {
  def center = line.center
  def absoluteFrom = line.a
  def absoluteTo = line.b
}

class StrategicMap(resources: Seq[ResourceArea], walkable: Grid2D, game: Game) {
  val domains = LazyVal.from({
    info(s"Analyzing map ${game.mapFileName()}")
    val tooMany = {
      val lineLength = 4
      val tries = (-lineLength, -lineLength) ::(0, -lineLength) ::(lineLength, -lineLength) ::(lineLength, 0) :: Nil map
                  { case (x, y) =>
                    val point = RelativePoint(x, y)
                    Line(point.asMapTile, point.opposite.asMapTile)
                  }

      val findSubAreasOfThis = walkable
      val myAreas = findSubAreasOfThis.areas
      myAreas.flatMap { area =>

        val relevantResources = resources.filter { r =>
          r.resources.exists(p => area.free(p.area.anyTile))
        }

        area.allContained.toVector.par.flatMap { center =>
          val cutters = tries.map(_.movedBy(center))
                        .filter(line => area.anyBlockedOnLine(line.movedBy(center)))
                        .flatMap { line =>
                          val operateOn = area.mutableCopy
                          operateOn.block_!(line)
                          operateOn.anyFree.map { _ =>
                            val modified = operateOn.areaCount
                            val untouched = area.areaCount
                            val cutsArea = modified != untouched
                            line -> cutsArea
                          }
                        }.collect { case (line, cuts) if cuts => line }
          if (cutters.nonEmpty) {
            val operateOn = area.mutableCopy
            cutters.foreach { line =>
              operateOn.block_!(line)
            }

            val subAreas = operateOn.areas
            val cuttingLines = cutters.map(CuttingLine)
            val chokePoint = ChokePoint(center, cuttingLines)
            val grouped = relevantResources.groupBy { r1 =>
              val mainArea = subAreas.find(_.free(r1.center))
              def ok = r1.resources.forall(e => subAreas.find(_.free(e.tilePosition)) == mainArea)
              if (ok) mainArea else None
            }
            if (grouped.values.forall(_.nonEmpty)) {
              val cleaned = grouped.filter(_._1.isDefined)
                            .map { case (k, v) => k.get -> v.toSet }
              if (cleaned.nonEmpty) {
                Some(chokePoint -> cleaned)
              } else {
                None
              }
            } else {
              None
            }
          } else
            None
        }.toVector
      }
    }

    val groupedByArea = tooMany.filter(_._2.size > 1)
                        .groupBy(_._2.values.toSet)
    val reduced = groupedByArea.values.map { manyWithSameValue =>
      val center = {
        val tup = manyWithSameValue.map { case (choke, _) =>
          choke.center.asTuple
        }.reduce((a, b) => (a._1 + b._1, a._2 + b._2))
        MapTilePosition.shared(tup._1 / manyWithSameValue.size, tup._2 / manyWithSameValue.size)
      }
      manyWithSameValue.minBy(_._1.center.distanceToSquared(center))
    }.toVector
    val complete = reduced.zipWithIndex.map { case ((choke, value), index) => choke.copy(index = index) -> value }
    complete
  })

  private def fileName = s"${game.mapFileName()}_${game.mapHash()}"
}