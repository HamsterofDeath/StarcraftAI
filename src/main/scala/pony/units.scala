package pony

import bwapi.{Unit => APIUnit, UnitType}

import scala.collection.immutable.HashMap

trait NiceToString extends WrapsUnit {
  override def toString = s"${getClass.getSimpleName}"
}

class AnyUnit(val nativeUnit: APIUnit) extends WrapsUnit with NiceToString {

}

trait WrapsUnit {
  def nativeUnit: APIUnit
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

}

trait SupplyProvider extends Building {

}

trait ResourceGatherPoint

trait MainBuilding extends Building with Factory with ResourceGatherPoint with SupplyProvider

trait Mobile extends WrapsUnit {
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
  def isMining = nativeUnit.isGatheringMinerals

}

trait Ignored extends WrapsUnit

trait Resource extends BlockingTiles {
  def remaining = nativeUnit.getResources
}

class MineralPatch(unit: APIUnit) extends AnyUnit(unit) with Resource

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