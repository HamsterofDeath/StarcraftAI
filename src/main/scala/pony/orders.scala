package pony

import bwapi.{Color, Game}
import pony.Upgrades.{SinglePointMagicSpell, SingleTargetMagicSpell}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

abstract class UnitOrder {
  def forceRepetition = forceAllowRepeats

  def forceRepeat_!(forceRepeats: Boolean) = {
    forceAllowRepeats = forceRepeats
    this
  }

  def lockTicks = locks

  def obsolete = !myUnit.isInGame

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

  private var locks             = 0
  private var forceAllowRepeats = false

  def lockingFor_!(ticks: Int) = {
    locks = ticks
    this
  }

}

object Orders {
  case class AttackUnit(attacker: MobileRangeWeapon, target: MaybeCanDie) extends UnitOrder {

    override def obsolete = super.obsolete || target.isDead
    override def myUnit: WrapsUnit = attacker
    override def issueOrderToGame(): Unit = attacker.nativeUnit.attack(target.nativeUnit)
    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Red).indicateTarget(attacker.currentPosition, target.center)
    }
  }

  case class TechOnSelf(caster: HasSingleTargetSpells, tech: SingleTargetMagicSpell) extends UnitOrder {
    override def myUnit: WrapsUnit = caster
    override def issueOrderToGame(): Unit = caster.nativeUnit.useTech(tech.nativeTech)
    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class TechOnTarget[T <: HasSingleTargetSpells](caster: HasSingleTargetSpells, target: Mobile,
                                                      tech: SingleTargetMagicSpell)
    extends UnitOrder {

    assert(tech.canCastOn.isAssignableFrom(target.getClass))

    override def myUnit = caster

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Red).indicateTarget(caster.currentPosition, target.currentTile)
    }

    override def issueOrderToGame(): Unit = {
      caster.nativeUnit.useTech(tech.nativeTech, target.nativeUnit)
    }
  }

  case class TechOnTile[T <: HasSinglePointMagicSpell](caster: HasSinglePointMagicSpell, target: MapTilePosition,
                                                       tech: SinglePointMagicSpell)
    extends UnitOrder {

    override def myUnit = caster

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Red).indicateTarget(caster.currentPosition, target)
    }

    override def issueOrderToGame(): Unit = {
      caster.nativeUnit.useTech(tech.nativeTech, target.asMapPosition.toNative)
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
      renderer.in_!(Color.White).drawOutline(basis.addonArea)
    }
  }

  case class ConstructBuilding(myUnit: WorkerUnit, buildingType: Class[_ <: Building], where: MapTilePosition)
    extends UnitOrder {
    private val buildingUnitType = buildingType.toUnitType

    val area = {
      val size = Size.shared(buildingUnitType.tileWidth(), buildingUnitType.tileHeight())
      Area(where, size)
    }

    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.build(buildingUnitType, where.asTilePosition)
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

  case class BoardFerry(myUnit: GroundUnit, ferry: TransporterUnit) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.rightClick(ferry.nativeUnit)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(myUnit.currentPosition, ferry.currentPosition)
    }
  }
  case class LoadUnit(ferry: TransporterUnit, loadThis: GroundUnit) extends UnitOrder {
    override def myUnit: WrapsUnit = ferry

    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.rightClick(ferry.nativeUnit)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(ferry.currentPosition, loadThis.currentPosition)
    }
  }

  case class UnloadAll(ferry: TransporterUnit, at: MapTilePosition) extends UnitOrder {
    override def myUnit: WrapsUnit = ferry

    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.unloadAll(at.asMapPosition.toNative)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(ferry.currentPosition, at)
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

  case class NoUpdate(unit: WrapsUnit) extends UnitOrder {

    override def isNoop: Boolean = true

    override def issueOrderToGame(): Unit = {}
    override def renderDebug(renderer: Renderer): Unit = {}
    override def myUnit = unit
  }
}

class OrderQueue(game: Game, debugger: Debugger) {
  private val queue              = ArrayBuffer.empty[UnitOrder]
  private val delegatedToBasicAI = collection.mutable.HashMap.empty[WrapsUnit, UnitOrder]
  private val locked             = collection.mutable.HashMap.empty[WrapsUnit, Int]

  def queue_!(order: UnitOrder): Unit = {
    order.setGame_!(game)
    queue += order
  }

  def debugAll():Unit = {
    if (debugger.isDebugging) {
      delegatedToBasicAI.filter(_._2.obsolete).foreach { dead =>
        delegatedToBasicAI.remove(dead._1)
      }
      queue.foreach { order =>
        delegatedToBasicAI.put(order.myUnit, order)
      }
      debugger.debugRender { renderer =>
        delegatedToBasicAI.foreach(_._2.renderDebug(renderer))
      }
    }
  }

  def issueAll():Unit = {
    val tickOrders = queue.filterNot(_.isNoop)
    trace(s"Orders: ${tickOrders.mkString(", ")}", queue.nonEmpty)
    tickOrders.foreach(_.record())
    tickOrders.foreach { order =>
      val isLocked = locked.get(order.myUnit).exists(_ > 0)
      if (isLocked) {
        locked.put(order.myUnit, locked(order.myUnit) - 1)
      } else {
        order.issueOrderToGame()
        locked.put(order.myUnit, order.lockTicks)
      }

    }
    queue.clear()
  }
}