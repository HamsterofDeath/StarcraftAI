package pony
package brain
package modules

import scala.reflect.ClassTag

class RepairDamagedBuilding(universe: Universe) extends DefaultBehaviour[SCV](universe) {

  private val helper = new NonConflictingTargets[TerranBuilding, SCV](
    universe = universe,
    rateTarget = m => PriorityChain(m.percentageHPOk),
    // a building BWAPI cannot place (an addon of a lifted factory reports an unknown position) has no ground to reach
    validTargetTest = t =>
      t.isInGame && t.tilePosition.isInsideOfGame && t.isDamaged && !t.isFloating &&
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
