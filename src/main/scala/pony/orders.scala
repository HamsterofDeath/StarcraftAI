package pony

import bwapi.{Color, Game}

import scala.collection.mutable.ArrayBuffer

abstract class UnitOrder {
  private var myGame:Game = _

  def setGame_!(game: Game):Unit = {
    myGame = game
  }

  def game = myGame

  def isNoop = false
  def myUnit: WrapsUnit
  def record(): Unit = {
    myUnit match {
      case his: OrderHistorySupport =>
        his.trackOrder(this)
      case _ =>
    }
  }
  def issueOrderToGame(): Unit
  def renderDebug(renderer: Renderer): Unit
}

object Orders {
  case class Construct(myUnit: WorkerUnit, buildingType: Class[_ <: Building], where: MapTilePosition)
    extends UnitOrder {
    val area = {
      val size = Size.shared(buildingType.toUnitType.tileWidth(), buildingType.toUnitType.tileHeight())
      Area(where, size)
    }

    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.build(where.asTilePosition, buildingType.toUnitType)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.White).drawOutline(area)
    }
  }

  case class Train(myUnit: UnitFactory, trainType: Class[_ <: Mobile]) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.train(trainType.toUnitType)
    }

    override def renderDebug(renderer: Renderer): Unit = {

    }
  }

  case class Move(myUnit: Mobile, to: MapTilePosition) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.move(to.asMapPosition.toNative)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(myUnit.currentPosition, to)
    }
  }
  case class AttackMove(myUnit: Mobile, where: MapTilePosition) extends UnitOrder {

    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.attack(where.asMapPosition.toNative)
    }

    override def renderDebug(renderer: Renderer): Unit = {

    }
  }

  case class Gather(myUnit: WorkerUnit, minsOrGas: Resource) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.gather(minsOrGas.nativeUnit)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Teal).indicateTarget(myUnit.currentPosition, minsOrGas.area)
    }
  }
  case class MoveToPatch(myUnit: WorkerUnit, patch: MineralPatch) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.move(patch.nativeMapPosition)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Blue).indicateTarget(myUnit.currentPosition, patch.area)
    }
  }
  case class Stop(myUnit: Mobile) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.stop()
    }

    override def renderDebug(renderer: Renderer): Unit = {

    }
  }
  case class ReturnMinerals(myUnit: WorkerUnit, to: MainBuilding) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      // for some reason, other commands are not reliable
      myUnit.nativeUnit.returnCargo()
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(myUnit.currentPosition, to.tilePosition)
    }
  }

  case object NoUpdate extends UnitOrder {

    override def isNoop: Boolean = true

    override def issueOrderToGame(): Unit = {}
    override def renderDebug(renderer: Renderer): Unit = {}
    override def myUnit: WrapsUnit = ???
  }
}

class OrderQueue(game: Game, debugger: Debugger) {
  private val queue = ArrayBuffer.empty[UnitOrder]

  def queue_!(order: UnitOrder): Unit = {
    order.setGame_!(game)
    queue += order
  }

  def debugAll():Unit = {
    debugger.debugRender { renderer =>
      queue.foreach(_.renderDebug(renderer))
    }
  }

  def issueAll():Unit = {
    val tickOrders = queue.filterNot(_.isNoop)
    trace(s"Orders: ${tickOrders.mkString(", ")}", queue.nonEmpty)
    tickOrders.foreach(_.record())
    tickOrders.foreach(_.issueOrderToGame())
    queue.clear()
  }
}