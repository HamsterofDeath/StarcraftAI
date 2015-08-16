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
  override def x = xOff
  override def y = yOff
}

case class CuttingLine(center: MapTilePosition, from: RelativePoint) {
  def absoluteFrom = center.movedBy(from)
  def absoluteTo = center.movedBy(to)
  def to = from.opposite
}

class StrategicMap(resources: Seq[ResourceArea], walkable: Grid2D, game: Game) {
  val domains = LazyVal.from({
    info(s"Analyzing map ${game.mapFileName()}")
    val tooMany = {
      val lineLength = 5
      val tries = (-lineLength, -lineLength) ::(0, -lineLength) ::(lineLength, -lineLength) ::(lineLength, 0) :: Nil map
                  { case (x, y) => RelativePoint(x, y) }

      val findSubAreasOfThis = walkable
      val myAreas = findSubAreasOfThis.areas
      myAreas.flatMap { area =>

        val relevantResources = resources.filter { r =>
          r.resources.exists(p => area.free(p.area.anyTile))
        }

        area.allContained.toVector.par.flatMap { center =>
          val cutters = tries.flatMap { line =>
            val operateOn = area.mutableCopy
            operateOn.blockLineRelative_!(center, line, line.opposite)
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
              operateOn.blockLineRelative_!(center, line, line.opposite)
            }

            val subAreas = operateOn.areas
            val cuttingLines = cutters.map { p => CuttingLine(center, p) }
            val chokePoint = ChokePoint(center, cuttingLines)
            val grouped = relevantResources.groupBy { r1 =>
              subAreas.find(_.containsAsData(r1.center))
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

    val groupedByArea = tooMany.groupBy(_._2.values.toSet)
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