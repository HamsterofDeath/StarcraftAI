package pony

import bwapi.{Color, TechType}
import pony.Upgrades.{SinglePointMagicSpell, SingleTargetMagicSpell}

import scala.util.Try

object Orders {
  private val bunkerBoardingReported = scala.collection.mutable.Map.empty[Int, (Boolean, Int, Int)]
  case class LiftDepot(myUnit: CommandCenter) extends UnitOrder {
    override def issueOrderToGame(): Unit              = { myUnit.nativeUnit.lift() }
    override def renderDebug(renderer: Renderer): Unit = {}
  }
  case class FlyDepot(myUnit: CommandCenter, to: MapTilePosition) extends UnitOrder {
    override def issueOrderToGame(): Unit              = { myUnit.nativeUnit.move(to.asMapPosition.toNative) }
    override def renderDebug(renderer: Renderer): Unit = {}
  }
  case class LandDepot(myUnit: CommandCenter, to: MapTilePosition) extends UnitOrder {
    override def issueOrderToGame(): Unit              = { myUnit.nativeUnit.land(to.asTilePosition) }
    override def renderDebug(renderer: Renderer): Unit = {}
  }

  /** Any Terran production building can lift, fly and land on another site. */
  case class LiftBuilding(myUnit: TerranBuilding) extends UnitOrder {
    override def issueOrderToGame(): Unit              = { myUnit.nativeUnit.lift() }
    override def renderDebug(renderer: Renderer): Unit = {}
  }
  case class FlyBuilding(myUnit: TerranBuilding, to: MapTilePosition) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      val accepted = myUnit.nativeUnit.move(to.asMapPosition.toNative)
      if (!accepted) NativeMatchEvidence.trace(
        "building-fly-refused",
        s"id=${myUnit.nativeUnitId} from=${myUnit.tilePosition} to=$to"
      )
    }
    override def renderDebug(renderer: Renderer): Unit = {}
  }
  case class LandBuilding(myUnit: TerranBuilding, to: MapTilePosition) extends UnitOrder {
    override def issueOrderToGame(): Unit              = { myUnit.nativeUnit.land(to.asTilePosition) }
    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class ScanWithComsat(comsat: Comsat, where: MapTilePosition) extends UnitOrder {
    override def myUnit = comsat

    override def issueOrderToGame() = {
      comsat.nativeUnit.useTech(TechType.Scanner_Sweep, where.nativeMapPosition)
    }

    override def renderDebug(renderer: Renderer) = {

      val red = renderer.in_!(Color.Red)
      for (x <- -10 to 10; y <- -10 to 10) {
        red.drawCircleAroundTile(where.movedBy(x, y))
      }
    }
  }

  case class AttackUnit(attacker: MobileRangeWeapon, target: CanDie) extends UnitOrder {

    override def obsolete = super.obsolete || target.isDead

    override def myUnit: WrapsUnit = attacker

    override def issueOrderToGame(): Unit = attacker.nativeUnit.attack(target.nativeUnit)

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Red).indicateTarget(attacker.currentPosition, target.center)
    }
  }

  case class TechOnSelf(caster: HasSingleTargetSpells, tech: SingleTargetMagicSpell)
      extends UnitOrder {
    override def myUnit: WrapsUnit = caster

    override def issueOrderToGame(): Unit = caster.nativeUnit.useTech(tech.nativeTech)

    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class TechOnTarget[T <: HasSingleTargetSpells](
      caster: HasSingleTargetSpells,
      target: Mobile,
      tech: SingleTargetMagicSpell
  ) extends UnitOrder {

    assert(tech.canCastOn.isInstance(target))

    override def myUnit = caster

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Red).indicateTarget(caster.currentPosition, target.currentTile)
    }

    override def issueOrderToGame(): Unit = {
      caster.nativeUnit.useTech(tech.nativeTech, target.nativeUnit)
    }
  }

  case class TechOnTile[T <: HasSinglePointMagicSpell](
      caster: HasSinglePointMagicSpell,
      target: MapTilePosition,
      tech: SinglePointMagicSpell
  ) extends UnitOrder {

    override def myUnit = caster

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Red).indicateTarget(caster.centerTile.asMapPosition, target)
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
        case Right(tech)   => basis.nativeUnit.research(tech)
      }
    }

    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class ConstructAddon(basis: CanBuildAddons, builtWhat: Class[? <: Addon]) extends UnitOrder {
    assert(Try(builtWhat.toUnitType).isSuccess)

    override def myUnit: WrapsUnit = basis

    override def issueOrderToGame(): Unit = {
      basis.nativeUnit.buildAddon(builtWhat.toUnitType)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.White).drawOutline(basis.addonArea)
    }
  }

  case class ConstructBuilding(
      myUnit: WorkerUnit,
      buildingType: Class[? <: Building],
      where: MapTilePosition
  ) extends UnitOrder {
    val area = {
      val size = Size.shared(buildingUnitType.tileWidth(), buildingUnitType.tileHeight())
      Area(where, size)
    }

    override def issueOrderToGame(): Unit = {
      val accepted = myUnit.nativeUnit.build(buildingUnitType, where.asTilePosition)
      if (!accepted && NativeMatchEvidence.firstRefusals(s"${buildingUnitType}@$where"))
        NativeMatchEvidence.trace(
          "build-refused",
          s"type=$buildingUnitType worker=${myUnit.nativeUnitId} from=${myUnit.currentTile} to=$where " +
            NativeMatchEvidence.buildDiagnosis(myUnit.nativeUnit, where.asTilePosition, buildingUnitType)
        )
    }

    private def buildingUnitType = buildingType.toUnitType

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.White).drawOutline(area)
      renderer.in_!(Color.White).drawLine(myUnit.currentPosition, area.center)
    }
  }

  case class Train(myUnit: UnitFactory, trainType: Class[? <: Mobile]) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.train(trainType.toUnitType)
    }

    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class MoveToTile(myUnit: Mobile, to: MapTilePosition) extends UnitOrder {
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

  case class EnterBunker(myUnit: Marine, bunker: Bunker) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      val accepted = myUnit.nativeUnit.rightClick(bunker.nativeUnit)
      val now      = myUnit.universe.currentTick
      val previous = bunkerBoardingReported.get(myUnit.nativeUnitId)
      if (!previous.exists(p => p._1 == accepted && p._2 == bunker.nativeUnitId && now - p._3 < 120)) {
        NativeMatchEvidence.trace(
          "bunker-boarding-order",
          s"marine=${myUnit.nativeUnitId} target=${bunker.nativeUnitId} accepted=$accepted order=${myUnit.nativeUnit.getOrder} loaded=${myUnit.nativeUnit.isLoaded}"
        )
        bunkerBoardingReported(myUnit.nativeUnitId) = (accepted, bunker.nativeUnitId, now)
      }
    }
    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class LoadUnit(ferry: TransporterUnit, loadThis: GroundUnit) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.rightClick(loadThis.nativeUnit)
    }

    override def myUnit: WrapsUnit = ferry

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(ferry.currentPosition, loadThis.currentPosition)
    }
  }

  case class UnloadUnit(ferry: TransporterUnit, dropThis: GroundUnit) extends UnitOrder {
    override def myUnit: WrapsUnit = ferry

    override def issueOrderToGame(): Unit = ferry.nativeUnit.unload(dropThis.nativeUnit)

    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class UnloadAll(ferry: TransporterUnit, at: MapTilePosition) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.unloadAll(at.asMapPosition.toNative)
    }

    override def myUnit: WrapsUnit = ferry

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.indicateTarget(ferry.currentPosition, at)
    }
  }

  case class AttackMove(myUnit: Mobile, where: MapTilePosition) extends UnitOrder {

    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.attack(where.asMapPosition.toNative)
    }

    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class ContinueConstruction(myUnit: SCV, what: Building) extends UnitOrder {

    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.rightClick(what.nativeUnit)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.White).drawOutline(what.area)
      renderer.in_!(Color.White).indicateTarget(myUnit.currentTile.asMapPosition, what.area)
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

  /** A spell on any unit, a building included (the Yamato gun). */
  case class UseTechOnUnit(myUnit: Mobile, target: bwapi.Unit, tech: TechType) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.useTech(tech, target)
    }

    override def renderDebug(renderer: Renderer): Unit = {}
  }

  /** Stay put, still firing at whatever comes in range. */
  case class HoldPosition(myUnit: Mobile) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.holdPosition()
    }

    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class Stop(myUnit: Mobile) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.stop()
    }

    override def renderDebug(renderer: Renderer): Unit = {}
  }

  case class ReturnResourcesToAnyBase(myUnit: WorkerUnit) extends UnitOrder {
    override def issueOrderToGame() = {
      myUnit.nativeUnit.returnCargo()
    }

    override def renderDebug(renderer: Renderer) = {}
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

  case class RepairUnit(myUnit: SCV, fixWhat: Mechanic) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      myUnit.nativeUnit.repair(fixWhat.nativeUnit)
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Blue).indicateTarget(myUnit.currentPosition, fixWhat.currentPosition)
    }
  }

  case class RepairBuilding(myUnit: SCV, fixWhat: TerranBuilding) extends UnitOrder {
    override def issueOrderToGame(): Unit = {
      val accepted = myUnit.nativeUnit.repair(fixWhat.nativeUnit)
      if (fixWhat.isInstanceOf[Bunker] && myUnit.universe.currentTick % 120 < 24)
        NativeMatchEvidence.trace(
          "bunker-native-repair",
          s"scv=${myUnit.nativeUnitId} bunker=${fixWhat.nativeUnitId} accepted=$accepted hp=${fixWhat.nativeUnit.getHitPoints} order=${myUnit.nativeUnit.getOrder}"
        )
    }

    override def renderDebug(renderer: Renderer): Unit = {
      renderer.in_!(Color.Blue).indicateTarget(myUnit.currentPosition, fixWhat.centerTile)
    }
  }

  case class NoUpdate(unit: WrapsUnit) extends UnitOrder {

    override def isNoop: Boolean = true

    override def issueOrderToGame(): Unit = {}

    override def renderDebug(renderer: Renderer): Unit = {}

    override def myUnit = unit
  }

}
