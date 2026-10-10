package pony

import pony.units.WrapsUnit

import bwapi.Game

import scala.collection.mutable.ArrayBuffer

class OrderQueue(game: Game, debugger: Debugger) {
  private val queue              = ArrayBuffer.empty[UnitOrder]
  private val delegatedToBasicAI = collection.mutable.HashMap.empty[WrapsUnit, UnitOrder]
  private val locked             = collection.mutable.HashMap.empty[WrapsUnit, Int]

  def queue_!(order: UnitOrder): Unit = {
    order.setGame_!(game)
    queue += order
  }

  def debugAll(): Unit = {
    if (debugger.isDebugging) {
      debugger.debugRender { renderer =>
        delegatedToBasicAI.foreach(_._2.renderDebug(renderer))
      }
    }
  }

  def issueAll(elapsedFrames: Int): Unit = {
    val tickOrders = queue.filterNot(_.isNoop)
    trace(s"Orders: ${tickOrders.mkString(", ")}", queue.nonEmpty)
    tickOrders.foreach(_.record())
    delegatedToBasicAI.clear()
    tickOrders.foreach { order =>
      delegatedToBasicAI.put(order.myUnit, order)
      val isLocked = locked.get(order.myUnit).exists(_ > 0)
      if (isLocked) {
        locked.put(order.myUnit, locked(order.myUnit) - elapsedFrames)
      } else {
        order.issueOrderToGame()
        locked.put(order.myUnit, order.lockTicks)
      }
    }
    queue.clear()
  }
}
