package pony

import bwapi.{Order, Race, Unit => APIUnit, UnitType}

import scala.collection.immutable.HashMap

trait NiceToString extends WrapsUnit {
  override def toString = s"[$unitIdText] ${getClass.className}"
}

class AnyUnit(val nativeUnit: APIUnit) extends WrapsUnit with NiceToString {

}

trait WrapsUnit {
  val unitId = WrapsUnit.nextId
  def nativeUnit: APIUnit
  def unitIdText = Integer.toString(unitId, 36)
  def race = {
    val r = nativeUnit.getType.getRace
    if (r == Race.Protoss) Protoss
    else if (r == Race.Terran) Terran
    else if (r == Race.Zerg) Zerg
    else Other
  }
  def isBeingCreated = nativeUnit.getRemainingBuildTime > 0
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
  val position              = tilePosition.asMapPosition
  val size                  = Size.shared(nativeUnit.getType.tileWidth(), nativeUnit.getType.tileHeight())
  val area                  = Area(tilePosition, size)
  val nativeMapTilePosition = tilePosition.asNative
  val nativeMapPosition     = tilePosition.asMapPosition.toNative

  override def toString: String = {
    s"${super.toString}@${area.describe}"
  }
}

trait BlockingTiles extends StaticallyPositioned {

}

trait Building extends BlockingTiles {

}

trait Upgrader {

}

trait UpgraderBuilding extends Building with Upgrader {

}

trait Addon extends Building {

}

trait UnitFactory extends Building {
  def canBuild[T <: Mobile](typeOfUnit: Class[_ <: T]) = {
    Dependencies.builderOf(typeOfUnit).isAssignableFrom(getClass)
  }

  def isProducing = nativeUnit.getOrder == Order.Train || nativeUnit.getRemainingTrainTime > 0
}

trait SupplyProvider extends WrapsUnit {

}

trait ResourceGatherPoint

trait MainBuilding extends Building with UnitFactory with ResourceGatherPoint with SupplyProvider

trait SpellcasterBuilding extends Building {

}

trait Mobile extends WrapsUnit {
  def isMoving = nativeUnit.isMoving

  def currentTileNative = currentTile.asNative
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
  def isConstructing = nativeUnit.getOrder == Order.ConstructingBuilding

}

trait TransporterUnit extends AirUnit {

}

trait Ignored extends WrapsUnit

trait Resource extends BlockingTiles {
  def remaining = nativeUnit.getResources
}

trait RangeWeapon {

}

trait ArmedBuilding extends Building with RangeWeapon

trait ImmobileSupplyProvider extends SupplyProvider with Building
trait MobileSupplyProvider extends SupplyProvider with Mobile

trait GasProvider extends Resource with Building

class MineralPatch(unit: APIUnit) extends AnyUnit(unit) with Resource {
  def isBeingMined = nativeUnit.isBeingGathered
}

class VespeneGeysir(unit: APIUnit) extends AnyUnit(unit) with Geysir {
}

class SupplyDepot(unit: APIUnit) extends AnyUnit(unit) with ImmobileSupplyProvider
class Pylon(unit: APIUnit) extends AnyUnit(unit) with ImmobileSupplyProvider
class Overlord(unit: APIUnit) extends AnyUnit(unit) with MobileSupplyProvider with TransporterUnit

class SCV(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit
class Probe(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit
class Drone(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit

class Shuttle(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit
class Transporter(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit

class CommandCenter(unit: APIUnit) extends AnyUnit(unit) with MainBuilding
class Comsat(unit: APIUnit) extends AnyUnit(unit) with SpellcasterBuilding with Addon
class NuclearSilo(unit: APIUnit) extends AnyUnit(unit) with SpellcasterBuilding with Addon
class PhysicsLab(unit: APIUnit) extends AnyUnit(unit) with UpgraderBuilding with Addon
class Refinery(unit: APIUnit) extends AnyUnit(unit) with GasProvider

class Barracks(unit: APIUnit) extends AnyUnit(unit) with UnitFactory
class Factory(unit: APIUnit) extends AnyUnit(unit) with UnitFactory
class Starport(unit: APIUnit) extends AnyUnit(unit) with UnitFactory

class Academy(unit: APIUnit) extends AnyUnit(unit) with UpgraderBuilding
class Armory(unit: APIUnit) extends AnyUnit(unit) with UpgraderBuilding
class ControlTower(unit: APIUnit) extends AnyUnit(unit) with UpgraderBuilding with Addon
class EngineeringBay(unit: APIUnit) extends AnyUnit(unit) with UpgraderBuilding
class MachineShop(unit: APIUnit) extends AnyUnit(unit) with UpgraderBuilding with Addon

class MissileTurret(unit: APIUnit) extends AnyUnit(unit) with ArmedBuilding

class Bunker(unit: APIUnit) extends AnyUnit(unit)

class Irrelevant(unit: APIUnit) extends AnyUnit(unit)

trait Geysir extends Resource

object UnitWrapper {
  private val mappingRules: Map[UnitType, APIUnit => WrapsUnit] =
    HashMap(
      UnitType.Resource_Vespene_Geyser -> (new VespeneGeysir(_)),

      UnitType.Terran_SCV -> (new SCV(_)),
      UnitType.Terran_Supply_Depot -> (new SupplyDepot(_)),
      UnitType.Terran_Command_Center -> (new CommandCenter(_)),
      UnitType.Terran_Barracks -> (new Barracks(_)),
      UnitType.Terran_Academy -> (new Academy(_)),
      UnitType.Terran_Armory -> (new Academy(_)),
      UnitType.Terran_Bunker -> (new Bunker(_)),
      UnitType.Terran_Comsat_Station -> (new Comsat(_)),
      UnitType.Terran_Control_Tower -> (new ControlTower(_)),
      UnitType.Terran_Engineering_Bay -> (new EngineeringBay(_)),
      UnitType.Terran_Factory -> (new Factory(_)),
      UnitType.Terran_Machine_Shop -> (new MachineShop(_)),
      UnitType.Terran_Missile_Turret -> (new MissileTurret(_)),
      UnitType.Terran_Nuclear_Silo -> (new NuclearSilo(_)),
      UnitType.Terran_Physics_Lab -> (new PhysicsLab(_)),
      UnitType.Terran_Refinery -> (new Refinery(_)),
      UnitType.Terran_Starport -> (new Starport(_)),

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

object TypeMapping {

  private val class2UnitType: Map[Class[_ <: WrapsUnit], UnitType] =
    Map(classOf[SCV] -> UnitType.Terran_SCV,
      classOf[SupplyDepot] -> UnitType.Terran_Supply_Depot,
      classOf[CommandCenter] -> UnitType.Terran_Command_Center,
      classOf[Irrelevant] -> UnitType.Unknown)

  def unitTypeOf[T <: WrapsUnit](c: Class[_ <: T]) = class2UnitType(c)
}