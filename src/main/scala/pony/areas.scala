package pony

import bwapi.Game
import pony.brain.Base

import scala.collection.mutable.ArrayBuffer

case class ResourceArea(patches: Option[MineralPatchGroup], geysirs: Set[Geysir]) {
  val resources = patches.map(_.patches).getOrElse(Nil) ++ geysirs
  def mineralsAndGas = resources.iterator.map(_.remaining).sum
  def center = patches.map(_.center).getOrElse(geysirs.head.tilePosition)
  def isPatchId(id: Int) = patches.fold(false)(_.patchId == id)
  lazy val coveredTiles             = resources.flatMap(_.area.tiles).toSet
  lazy val mostAnnoyingMinePosition = center
}

case class PotentialDomain(coveredOnLand: Seq[ResourceArea], needsToControl: Seq[MapTilePosition])
case class ChokePoint(center: MapTilePosition, lines: Seq[CuttingLine], index: Int = -1)
case class NarrowPoint(where: MapTilePosition, index: Int)

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

class StrategicMap(val resources: Seq[ResourceArea], walkable: Grid2D, game: Game) {

  case class FrontLine(chokePoint: ChokePoint,
                       defendedAreas: Map[Grid2D, Set[ResourceArea]],
                       defended: Grid2D,
                       outerTerritory: Grid2D,
                       distanceBetweenTiles: Int) {
    def center = chokePoint.center

    def isDefenseLine = outerTerritory.freeCount > defended.freeCount

    def tileDistance(distance: Int) = copy(distanceBetweenTiles = distance)

    lazy val mergedArea = {
      val mut = walkable.blockedMutableCopy
      mut.or_!(defended.mutableCopy)
      mut.asReadOnlyView
    }

    private def evalScatteredPoints(valid: MapTilePosition => Boolean) = {
      val blocked = walkable.mutableCopy
      val pf = PathFinder.on(walkable)
      val outsidePoints = walkable.spiralAround(chokePoint.center, 20)
                          .filter(blocked.free)
                          .filter(valid)
                          .filter { e =>
                            val solution = pf.findPathNow(e, chokePoint.center)
                            solution.isPerfectSolution && solution.length <= 20
                          }
      val mineTiles = ArrayBuffer.empty[MapTilePosition]
      outsidePoints.foreach { p =>
        if (blocked.free(p)) {
          p.asArea.extendedBy(distanceBetweenTiles).tiles.foreach(blocked.block_!)
          mineTiles += p
        }
      }
      mineTiles.sortBy(_.distanceSquaredTo(chokePoint.center)).toVector
    }

    private lazy val scatteredPointsOutside = BWFuture(evalScatteredPoints(mergedArea.blocked), Vector.empty)
    private lazy val scatteredPointsInside  = BWFuture(evalScatteredPoints(mergedArea.free), Vector.empty)

    def pointsOutside = scatteredPointsOutside.result

    def pointsInside = scatteredPointsInside.result
  }

  def defenseLineOf(base: Base): Option[FrontLine] = defenseLineOf(base.mainBuilding.area.centerTile)

  def defenseLineOf(tile: MapTilePosition): Option[FrontLine] = {
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
      val (inside, outside) = areas.partition(_._1.free(tile))
      val insideArea = {
        val ret = walkable.emptySameSize(false)
        inside.keys.foreach { section =>
          ret.or_!(section.mutableCopy)
        }
        ret
      }
      val outsideArea = {
        val ret = walkable.emptySameSize(false)
        outside.keys.foreach { section =>
          ret.or_!(section.mutableCopy)
        }
        ret
      }
      FrontLine(choke, areas, insideArea.asReadOnlyCopyIfMutable, outsideArea.asReadOnlyCopyIfMutable, 1)
    }.filter {_.isDefenseLine}
    defLine
  }

  private val myNarrowPassages = FileStorageLazyVal.fromFunction({
    info(s"Calculating narrow passages...")
    val tooMany = {
      val lines = tryCutters(8).map { e => e.split }
      walkable.allFree.toVector.par.flatMap { freeSpot =>
        lines.map { case (l, r) => l.movedBy(freeSpot) -> r.movedBy(freeSpot) }
        .filter { case (left, right) =>
          walkable.anyBlockedOnLine(left) &&
          walkable.anyBlockedOnLine(right)
        }
      }.map { case (a, b) => Line(a.a, b.b) }.seq
    }
    val byReferencePoint = multiMap[MapTilePosition, Line]
    tooMany.foreach { line =>
      val close = byReferencePoint.filter(_._1.distanceTo(line.center) <= 15)
                  .minByOpt(_._1.distanceTo(line.center))
      close match {
        case Some((pos, _)) =>
          byReferencePoint.addBinding(pos, line)
        case None =>
          byReferencePoint.addBinding(line.center, line)
      }
    }
    byReferencePoint.map { case (_, lines) =>
      val sum = lines.foldLeft((0, 0))((acc, e) => (acc._1 + e.center.x, acc._2 + e.center.y))
      MapTilePosition.shared(sum._1 / lines.size, sum._2 / lines.size)
    }.zipWithIndex.map { case (where, index) =>
      NarrowPoint(where, index)
    }

  }, s"narrow_${game.suggestFileName}")

  private def tryCutters(size: Int) = {
    val lineLength = size
    (-lineLength, -lineLength) ::(0, -lineLength) ::(lineLength, -lineLength) ::(lineLength, 0) :: Nil map
    { case (x, y) =>
      val point = RelativePoint(x, y)
      Line(point.asMapTile, point.opposite.asMapTile)
    }
  }

  private val mapData = FileStorageLazyVal.fromFunction({
    val charsInLine = 80
    val tilesPerChunk = 100

    info(s"Analyzing map ${game.mapFileName()}")
    @volatile var checked = 0
    val tooMany = {
      val tries = tryCutters(6)

      val findSubAreasOfThis = walkable
      val myAreas = findSubAreasOfThis.areas
      myAreas.par.flatMap { area =>

        val relevantResources = resources.filter { r =>
          r.resources.exists(p => area.free(p.area.anyTile))
        }

        val freeSpotsToCheck = area.allFree.toVector
        freeSpotsToCheck.par.flatMap { center =>
          checked += 1
          if (checked % tilesPerChunk == 0) {
            print(".")
          }
          if (checked % (tilesPerChunk * charsInLine) == 0) {
            println()
          }

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
              val mainArea = subAreas.find(subArea => r1.coveredTiles.exists(subArea.free))
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

    println("\nGrouping choke points...")
    val onlyUseful = tooMany.filter(_._2.size > 1)
    val groupedByArea = onlyUseful
                        .groupBy(_._2.values.toSet)
    val reduced = groupedByArea.values.map { manyWithSameValue =>
      val center = {
        val tup = manyWithSameValue.map { case (choke, _) =>
          choke.center.asTuple
        }.reduce((a, b) => (a._1 + b._1, a._2 + b._2))
        MapTilePosition.shared(tup._1 / manyWithSameValue.size, tup._2 / manyWithSameValue.size)
      }
      manyWithSameValue.minBy(_._1.center.distanceSquaredTo(center))
    }.toVector
    val complete = reduced.zipWithIndex.map { case ((choke, value), index) => choke.copy(index = index) -> value }
    val ret = complete.map { case (choke, map) =>
      choke -> map.map { case (area, res) => area -> res.map(_.coveredTiles) }
    }
    println("Done!")
    ret
  }, s"chokepoints_${game.suggestFileName}")

  private val myDomains = LazyVal.from {
    val raw = mapData.get
    raw.map { case (choke, map) =>
      choke -> map.map { case (area, coveredTiles) =>
        area -> coveredTiles.map(tiles => resources.find(_.coveredTiles == tiles).get)
      }
    }
  }

  def domains = myDomains.get

  def narrowPoints = myNarrowPassages.get

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