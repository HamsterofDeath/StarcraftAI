package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

trait OrderHistorySupport extends WrapsUnit {
  private val history                    = ListBuffer.empty[HistoryElement]
  private val maxHistory                 = if (memoryHog) 1000 else 24
  def trackOrder(order: UnitOrder): Unit = {
    history.lastOption.foreach(_.trackOrder_!(order))
  }
  override def onTick_!(): Unit = {
    super.onTick_!()
    if (universe.unitManager.hasJob(this)) {
      history += HistoryElement(
        nativeUnit.getOrder,
        nativeUnit.getOrderTarget,
        universe.unitManager.jobOf(this)
      )
      if (history.size > maxHistory) {
        history.remove(0)
      }
    }
  }

  def unitHistory = history.reverseIterator

  case class HistoryElement(order: Order, target: APIUnit, job: UnitWithJob[? <: WrapsUnit]) {
    private var issuedOrder: UnitOrder = uninitialized

    def trackOrder_!(issuedOrder: UnitOrder): Unit = {
      this.issuedOrder = issuedOrder
    }

    override def toString: String = s"$order, $issuedOrder, $target, $job"
  }

}
