package pony

import bwapi.{Order, Unit => APIUnit, UnitType}

import scala.collection.immutable.HashMap

trait NiceToString extends WrapsUnit {
  override def toString = s"[$unitIdText] ${getClass.getSimpleName}"
}

class AnyUnit(val nativeUnit: APIUnit) extends WrapsUnit with NiceToString {

}

trait WrapsUnit {
  val unitId = WrapsUnit.nextId
  def nativeUnit: APIUnit
  def unitIdText = Integer.toString(unitId, 36)
}

object WrapsUnit {
  private var counter = 0

  def nextId = {
    val ret = counter
    counter += 1
    ret
  }
}

trait StaticallyPositioned extends WrapsUnit {
  val tilePosition          = MapTilePosition.shared(nativeUnit.getTilePosition.getX, nativeUnit.getTilePosition.getY)
  val position              = tilePosition.toMapPosition
  val size                  = Size.shared(nativeUnit.getType.tileWidth(), nativeUnit.getType.tileHeight())
  val area                  = Area(tilePosition, size)
  val nativeMapTilePosition = tilePosition.toNative
  val nativeMapPosition     = tilePosition.toMapPosition.toNative

  override def toString: String = {
    s"${super.toString}@${area.describe}"
  }
}

trait BlockingTiles extends StaticallyPositioned {

}

trait Building extends BlockingTiles {

}

trait Factory extends Building {
  def canBuild[T <: Mobile](typeOfUnit: Class[_ <: T]) = {
    Dependencies.builderOf(typeOfUnit).isAssignableFrom(getClass)
  }
}

trait SupplyProvider extends Building {

}

trait ResourceGatherPoint

trait MainBuilding extends Building with Factory with ResourceGatherPoint with SupplyProvider

trait Mobile extends WrapsUnit {
  def isMoving = nativeUnit.isMoving

  def currentTileNative = currentTile.toNative
  def currentPositionNative = currentPosition.toNative
  def currentPosition = {
    val p = nativeUnit.getPosition
    MapPosition(p.getX, p.getY)
  }
  override def toString = s"${super.toString}@$currentTile"
  def currentTile = {
    val tp = nativeUnit.getTilePosition
    MapTilePosition.shared(tp.getX, tp.getY)
  }
}

trait Killable {

}

trait Detector {

}

trait AirUnit extends Killable with Mobile {

}

trait GroundUnit extends Killable with Mobile {

}

trait WorkerUnit extends Killable with Mobile {

  def isCarryingMinerals = nativeUnit.isCarryingMinerals
  def isInMiningProcess = nativeUnit.getOrder == Order.MiningMinerals
  def isWaitingForMinerals = nativeUnit.getOrder == Order.WaitForMinerals
  def isMovingToMinerals = nativeUnit.getOrder == Order.MoveToMinerals

}

trait Ignored extends WrapsUnit

trait Resource extends BlockingTiles {
  def remaining = nativeUnit.getResources
}

class MineralPatch(unit: APIUnit) extends AnyUnit(unit) with Resource {
  def isBeingMined = nativeUnit.isBeingGathered
}

class SCV(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit
class Probe(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit
class Drone(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit

class CommandCenter(unit: APIUnit) extends AnyUnit(unit) with MainBuilding

class Irrelevant(unit: APIUnit) extends AnyUnit(unit)

trait Geysir extends Resource

object UnitWrapper {
  private val mappingRules: Map[UnitType, APIUnit => WrapsUnit] =
    HashMap(
      UnitType.Terran_SCV -> (new SCV(_)),
      UnitType.Terran_Command_Center -> (new CommandCenter(_)),

      UnitType.Protoss_Probe -> (new Probe(_)),

      UnitType.Zerg_Drone -> (new Drone(_)),

      UnitType.Unknown -> (new Irrelevant(_)))

  def lift(unit: APIUnit) = {
    mappingRules.get(unit.getType) match {
      case Some(mapper) => mapper(unit)
      case None =>
        if (unit.getType.isMineralField) new MineralPatch(unit)
        else new Irrelevant(unit)
    }

  }
}

// this should be a part of bwmirror, but it's missing for some reason
object Dependencies {
  private val builtBy: Map[Class[_ <: WrapsUnit], Class[_ <: WrapsUnit]] = Map(classOf[SCV] -> classOf[CommandCenter])

  def builderOf(unitClass: Class[_ <: WrapsUnit]) = builtBy(unitClass)
}