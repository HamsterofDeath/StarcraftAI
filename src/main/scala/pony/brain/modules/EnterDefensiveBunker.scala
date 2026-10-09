package pony
package brain
package modules

import scala.reflect.ClassTag

class EnterDefensiveBunker(universe: Universe) extends DefaultBehaviour[Marine](universe) {
  override def priority                         = SecondPriority.Max
  override def forceRepeatedCommands            = true
  override protected def wrapBase(unit: Marine) = new SingleUnitBehaviour[Marine](unit, meta) {
    private val retry                                     = new BunkerBoardingRetry
    override def describeShort                            = "Garrison bunker"
    override def toOrder(what: Objective): Seq[UnitOrder] = {
      if (this.unit.nativeUnit.isLoaded) Orders.NoUpdate(this.unit).toList
      else this.universe.pluginByType[TerranBunkerDefense].bunkerFor(this.unit).map { b =>
        val heading = Option(this.unit.nativeUnit.getOrderTarget).exists(_.getID == b.nativeUnitId)
        if (retry.issue(currentTick, this.unit.currentTile, false, heading, this.unit.nativeUnit.isMoving))
          Orders.EnterBunker(this.unit, b)
        else Orders.NoUpdate(this.unit)
      }.toList
    }
  }
}
