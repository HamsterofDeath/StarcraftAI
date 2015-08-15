package pony

case class ResourceArea(patches: MineralPatchGroup, geysirs: Seq[Geysir]) {
  val resources = patches.patches ++ geysirs
  def center = patches.center
}

case class PotentialDomain(coveredOnLand: Seq[ResourceArea], needsToControl: Seq[MapTilePosition])
case class ChokePoint(center: MapTilePosition, lines: Seq[CuttingLine])

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

class StrategicMap(resources: Seq[ResourceArea], walkable: Grid2D) {
  val domains = {

    val lineLength = 4
    val tries = (-lineLength, -lineLength) ::(0, -lineLength) ::(lineLength, -lineLength) ::(lineLength, 0) :: Nil map
                { case (x, y) => RelativePoint(x, y) }

    val findSubAreasOfThis = walkable
    val myAreas = findSubAreasOfThis.areas
    myAreas.flatMap { area =>

      val relevantResources = resources.filter { r =>
        r.resources.exists(p => area.contains(p.area.anyTile))
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
            subAreas.find(_.contains(r1.center))
          }
          if (grouped.values.forall(_.nonEmpty)) {
            val cleaned = grouped.filter(_._1.isDefined)
                          .map { case (k, v) => k.get -> v }
                          .filter(_._1.containedCount > 100)
            Some(chokePoint -> cleaned)
          } else {
            None
          }
        } else
          None
      }.toVector
    }
  }
}