package pony

import bwapi.{Color, Game, TechType}
import pony.Upgrades.SingleTargetMagicSpell

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

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
  case class Stimpack(u: CanUseStimpack) extends UnitOrder {
    override def myUnit: WrapsUnit = u
    override def issueOrderToGame(): Unit = {
      u.nativeUnit.useTech(TechType.Stim_Packs)
    }
    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Red).drawTextAtMobileUnit(u, "Stim!", -1)
    }
  }

  object TechOnTarget {
    def ghostStop(ghost: Ghost, on: Mechanic) = TechOnTarget(ghost, on, Upgrades.Terran.GhostStop)
  }

  case class TechOnTarget[T <: HasSingleTargetSpells](caster: HasSingleTargetSpells, target: Mobile,
                                                      tech: SingleTargetMagicSpell)
    extends UnitOrder {

    override def myUnit = caster

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Red).indicateTarget(caster.currentPosition, target.currentTile)
    }

    override def issueOrderToGame(): Unit = {
      caster.nativeUnit.useTech(tech.asNativeTech, target.nativeUnit)
    }
  }

  case class Research(basis: Upgrader, what: Upgrade) extends UnitOrder {
    override def myUnit: WrapsUnit = basis
    override def issueOrderToGame(): Unit = {
      what.nativeType match {
        case Left(upgrade) => basis.nativeUnit.upgrade(upgrade)
        case Right(tech) => basis.nativeUnit.research(tech)
      }
    }
    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class ConstructAddon(basis: CanBuildAddons, builtWhat: Class[_ <: Addon]) extends UnitOrder {
    assert(Try(builtWhat.toUnitType).isSuccess)
    override def myUnit: WrapsUnit = basis
    override def issueOrderToGame(): Unit = {
      basis.nativeUnit.buildAddon(builtWhat.toUnitType)
    }
    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.White).drawOutline(basis.area)
    }
  }

  case class ConstructBuilding(myUnit: WorkerUnit, buildingType: Class[_ <: Building], where: MapTilePosition)
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
      renderer.in_!(Color.White).drawLine(myUnit.currentPosition, area.center)
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