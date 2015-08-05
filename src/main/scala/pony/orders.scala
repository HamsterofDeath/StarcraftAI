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

  def issueOrderToGame(): Unit
  def renderDebug(renderer: Renderer): Unit
}

object Orders {
  case class Construct(unit: WorkerUnit, buildingType: Class[_ <: Building], where: Area) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      unit.nativeUnit.build(where.upperLeft.toTilePosition, buildingType.toUnitType)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.White).drawOutline(where)
    }
  }

  case class Train(unit: Factory, trainType: Class[_ <: Mobile]) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      unit.nativeUnit.train(trainType.toUnitType)
    }

    override def renderDebug(renderer: Renderer): Unit = {

    }
  }

  case class Move(unit: Mobile, to: MapTilePosition) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      unit.nativeUnit.move(to.toMapPosition.toNative)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(unit.currentPosition, to)
    }
  }
  case class Gather(unit: WorkerUnit, patch: MineralPatch) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      unit.nativeUnit.gather(patch.nativeUnit)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Teal).indicateTarget(unit.currentPosition, patch.area)
    }
  }
  case class MoveToPatch(unit: WorkerUnit, patch: MineralPatch) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      unit.nativeUnit.move(patch.nativeMapPosition)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Blue).indicateTarget(unit.currentPosition, patch.area)
    }
  }
  case class Stop(unit: Mobile) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      unit.nativeUnit.stop()
    }

    override def renderDebug(renderer: Renderer): Unit = {

    }
  }
  case class ReturnMinerals(unit: WorkerUnit, to: MainBuilding) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      unit.nativeUnit.move(to.nativeMapPosition)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(unit.currentPosition, to.tilePosition)
    }
  }

  case object NoUpdate extends UnitOrder {

    override def isNoop: Boolean = true

    override def issueOrderToGame(): Unit = {}
    override def renderDebug(renderer: Renderer): Unit = {}
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
    queue.foreach(_.issueOrderToGame())
    queue.clear()
  }
}