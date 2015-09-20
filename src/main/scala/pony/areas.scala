package pony

import bwapi.Game
import pony.brain.Base

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

case class ResourceArea(patches: Option[MineralPatchGroup], geysirs: Set[Geysir]) {
  val resources = patches.map(_.patches).getOrElse(Nil) ++ geysirs
  def center = patches.map(_.center).getOrElse(geysirs.head.tilePosition)
  def isPatchId(id:Int) = patches.fold(false)(_.patchId == id)
  lazy val coveredTiles = resources.flatMap(_.area.tiles).toSet
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

  case class DefenseLine(chokePoint: ChokePoint, defendedAreas: Map[Grid2D, Set[ResourceArea]], defended: Grid2D,
                         distanceBetweenTiles: Int) {

    def tileDistance(distance: Int) = copy(distanceBetweenTiles = distance)

    lazy val mergedArea = {
      val mut = walkable.blockedMutableCopy
      mut.or_!(defended.mutableCopy)
      mut.asReadOnly
    }

    private def evalScatteredPoints(valid: MapTilePosition => Boolean) = {
      val blocked = walkable.mutableCopy
      val outsidePoints = walkable.spiralAround(chokePoint.center, 20)
                          .filter(valid)
      val mineTiles = ArrayBuffer.empty[MapTilePosition]
      outsidePoints.foreach { p =>
        if (blocked.free(p)) {
          p.asArea.extendedBy(distanceBetweenTiles).tiles.foreach(blocked.block_!)
          mineTiles += p
        }
      }
      mineTiles.sortBy(_.distanceToSquared(chokePoint.center)).toVector
    }

    import scala.concurrent.ExecutionContext.Implicits.global

    private lazy val scatteredPointsOutside = Future {evalScatteredPoints(mergedArea.blocked)}
    private lazy val scatteredPointsInside  = Future {evalScatteredPoints(mergedArea.free)}

    def pointsOutside = scatteredPointsOutside.getOrElse(Vector.empty)

    def pointsInside = scatteredPointsInside.getOrElse(Vector.empty)
  }

  def defenseLineOf(base: Base): Option[DefenseLine] = defenseLineOf(base.mainBuilding.area.centerTile)

  def defenseLineOf(tile: MapTilePosition): Option[DefenseLine] = {
    val cutOffBy = {
      val candidates = domains.filter(_._2.keysIterator.exists(_.free(tile)))
      if (candidates.nonEmpty) {
        val smallest = candidates.minBy(_._2.keysIterator.find(_.free(tile)).get.freeCount)
        Some(smallest)
      } else {
        None
      }
    }

    val defLine = cutOffBy.map { case (choke, areas) =>
      val defendedArea = areas.find(_._1.free(tile)).get._1
      DefenseLine(choke, areas, defendedArea, 1)
    }
    defLine
  }

  private val mapData = FileStorageLazyVal.from({
    info(s"Analyzing map ${game.mapFileName()}")
    val tooMany = {
      val lineLength = 6
      val tries = (-lineLength, -lineLength) ::(0, -lineLength) ::(lineLength, -lineLength) ::(lineLength, 0) :: Nil map
                  { case (x, y) =>
                    val point = RelativePoint(x, y)
                    Line(point.asMapTile, point.opposite.asMapTile)
                  }

      val findSubAreasOfThis = walkable
      val myAreas = findSubAreasOfThis.areas
      myAreas.par.flatMap { area =>

        val relevantResources = resources.filter { r =>
          r.resources.exists(p => area.free(p.area.anyTile))
        }

        val freeSpotsToCheck = area.allFree.toVector
        freeSpotsToCheck.par.flatMap { center =>
          val cutters = tries.map(_.movedBy(center))
                        .filter(line => area.anyBlockedOnLine(line))
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
        }.seq
      }.seq
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
    complete.map { case (choke, map) =>
      choke -> map.map { case (area, resources) => area -> resources.map(_.coveredTiles) }
    }
  }, s"mapdata_${game.mapHash()}_${game.mapName()}")

  private val myDomains = LazyVal.from {
    val raw = mapData.get
    raw.map { case (choke, map) =>
      choke -> map.map { case (area, coveredTiles) =>
        area -> coveredTiles.map(tiles => resources.find(_.coveredTiles == tiles).get)
      }
    }
  }

  def domains = myDomains.get

  def domainsButWithout(these: Set[ResourceArea]) = {
    domains.flatMap { case (choke, areas) =>
      val cleaned = areas.flatMap { case (grid, resourcesInArea) =>
        val remaining = resourcesInArea -- these
        if (remaining.nonEmpty) {
          Some(grid -> remaining)
        } else
          None
      }
      if (cleaned.nonEmpty) {
        Some(choke -> cleaned)
      } else {
        None
      }
    }
  }

  private def fileName = s"${game.mapFileName()}_${game.mapHash()}"
}