package pony
package brain
package modules
package economy

import pony.brain.modules.micro.NonConflictingTargets
import pony.brain.modules.wall.WallWithDepots
import pony.units.{SCV, TerranBuilding}

import scala.reflect.ClassTag

class RepairDamagedBuilding(universe: Universe) extends DefaultBehaviour[SCV](universe) {

  private val helper = new NonConflictingTargets[TerranBuilding, SCV](
    universe = universe,
    rateTarget = m => PriorityChain(m.percentageHPOk),
    // a building BWAPI cannot place (an addon of a lifted factory or command center reports an unknown position,
    // tile 1000,1002, which isInsideOfGame lets through) has no ground to reach
    validTargetTest = t =>
      t.isInGame && t.tilePosition.isInsideOfGame && universe.mapLayers.rawWalkableMap.areaOf(t.centerTile).isDefined &&
        t.isDamaged && !t.isFloating &&
        !universe.pluginByType[WallWithDepots].demolishing(t.nativeUnitId),
    subAccept = (m, t) => !t.isFloating && m.currentArea.contains(t.areaOnMap),
    subRate = (m, t) => PriorityChain(-m.currentTile.distanceSquaredTo(t.centerTile)),
    own = true,
    allowReplacements = true
  )

  override def priority = SecondPriority.EvenMore

  override def onTick_!() = {
    super.onTick_!()
    helper.onTick_!()
  }

  override protected def wrapBase(unit: SCV) = new SingleUnitBehaviour[SCV](unit, meta) {
    override def onStealUnit() = {
      super.onStealUnit()
      helper.unlock_!(this.unit)
    }

    override def describeShort: String = "Repair building"

    override def toOrder(what: Objective): Seq[UnitOrder] = {
      helper.suggestTarget(this.unit).map { what =>
        Orders.RepairBuilding(this.unit, what)
      }.toList
    }
  }
}
