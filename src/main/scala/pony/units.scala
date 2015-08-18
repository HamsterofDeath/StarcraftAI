package pony

import bwapi.{Order, Race, Unit => APIUnit, UnitType}
import pony.brain.{PriorityChain, UnitWithJob, Universe}

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait HasNativeSCAttributes {
  def nativeUnit: APIUnit
  def currentOrder = nativeUnit.getOrder
}

trait NiceToString extends WrapsUnit {
  override def toString = s"[$unitIdText] ${getClass.className}"
}

class AnyUnit(val nativeUnit: APIUnit) extends WrapsUnit with NiceToString with OrderHistorySupport {
}

trait OrderHistorySupport extends WrapsUnit {
  private val history    = ListBuffer.empty[HistoryElement]
  private val maxHistory = 1000
  override def onTick(universe: Universe): Unit = {
    super.onTick(universe)
    history += HistoryElement(nativeUnit.getOrder, nativeUnit.getOrderTarget, universe.unitManager.jobOf(this))
    if (history.size > maxHistory) {
      history.remove(0)
    }
  }
  def unitHistory = history.reverse
  case class HistoryElement(order: Order, target: APIUnit, job: UnitWithJob[_ <: WrapsUnit])

}

trait WrapsUnit extends HasNativeSCAttributes {
  val unitId      = WrapsUnit.nextId
  val initialType = nativeUnit.getType
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
  def onTick(universe: Universe) = {

  }

}

trait Controllable extends WrapsUnit

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

trait Upgrader extends Controllable {

}

trait UpgraderBuilding extends Building with Upgrader {

}

trait Addon extends Building {

}

trait UnitFactory extends Building with Controllable {
  def canBuild[T <: Mobile](typeOfUnit: Class[_ <: T]) = {
    Dependencies.builderOf(typeOfUnit).isAssignableFrom(getClass)
  }

  def isProducing = nativeUnit.getOrder == Order.Train || nativeUnit.getRemainingTrainTime > 0
}

trait SupplyProvider extends WrapsUnit {

}

trait ResourceGatherPoint

trait MainBuilding extends Building with UnitFactory with ResourceGatherPoint with SupplyProvider

trait SpellcasterBuilding extends Building with Controllable {

}

trait Mobile extends WrapsUnit with Controllable {
  def isGuarding = nativeUnit.getOrder == Order.PlayerGuard

  def isMoving = nativeUnit.isMoving

  def currentTileNative = currentTile.asNative
  def currentTile = {
    val tp = nativeUnit.getPosition
    MapTilePosition.shared(tp.getX / 32, tp.getY / 32)
  }
  def currentPositionNative = currentPosition.toNative
  def currentPosition = {
    val p = nativeUnit.getPosition
    MapPosition(p.getX, p.getY)
  }
  override def toString = s"${super.toString}@$currentTile"
}

trait Killable {

}

trait Detector {

}

trait ArmedUnit extends WrapsUnit {

}

trait SpiderMines extends WrapsUnit

trait GroundWeapon extends Weapon {

}

trait AirWeapon extends Weapon {

}

trait Weapon extends Controllable {

}

trait RangeWeapon extends Weapon {

}

trait MeleeWeapon extends Weapon {

}

trait GroundAndAirWeapon extends RangeWeapon with GroundWeapon with AirWeapon {

}

trait AirUnit extends Killable with Mobile {

}

trait GroundUnit extends Killable with Mobile {

}

trait Floating

trait WorkerUnit extends Killable with Mobile with GroundUnit with GroundWeapon with Floating {
  def isGatheringGas = currentOrder == Order.HarvestGas || currentOrder == Order.MoveToGas ||
                       currentOrder == Order.ReturnGas || currentOrder == Order.WaitForGas

  def isMagic = currentOrder == Order.ResetCollision
  def isInMiningProcess = currentOrder == Order.MiningMinerals
  def isWaitingForMinerals = currentOrder == Order.WaitForMinerals
  def isMovingToMinerals = currentOrder == Order.MoveToMinerals
  def isInConstructionProcess = isConstructingBuilding || currentOrder == Order.PlaceBuilding
  def isConstructingBuilding = currentOrder == Order.ConstructingBuilding
  def isCarryingNothing = !isCarryingMinerals && !isCarryingGas
  def isCarryingMinerals = nativeUnit.isCarryingMinerals
  def isCarryingGas = nativeUnit.isCarryingGas

}

object WorkerUnit {
  def currentPriority[T <: WorkerUnit](w: UnitWithJob[T]) = {
    var valueOfExcuses = 0
    if (!w.isIdle) valueOfExcuses += 1
    if (w.unit.isCarryingMinerals) valueOfExcuses += 2
    if (w.unit.isInMiningProcess) valueOfExcuses += 1
    PriorityChain(valueOfExcuses)
  }
}

trait TransporterUnit extends AirUnit {

}

trait Ignored extends WrapsUnit

trait Resource extends BlockingTiles {
  def remaining = nativeUnit.getResources
}

trait ArmedBuilding extends Building with RangeWeapon

trait ImmobileSupplyProvider extends SupplyProvider with Building
trait MobileSupplyProvider extends SupplyProvider with Mobile

trait GasProvider extends Resource with Building

class MineralPatch(unit: APIUnit) extends AnyUnit(unit) with Resource {
  def isBeingMined = nativeUnit.isBeingGathered
}

class VespeneGeysir(unit: APIUnit) extends AnyUnit(unit) with Geysir with Resource {
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

class Marine(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon
class Firebat(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon
class Ghost(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon
class Medic(unit: APIUnit) extends AnyUnit(unit) with GroundUnit
class Vulture(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon with SpiderMines
class Tank(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon
class Goliath(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon
class Wraith(unit: APIUnit) extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon
class Valkery(unit: APIUnit) extends AnyUnit(unit) with AirUnit with AirWeapon
class Battlecruiser(unit: APIUnit) extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon
class ScienceVessel(unit: APIUnit) extends AnyUnit(unit) with AirUnit

class Irrelevant(unit: APIUnit) extends AnyUnit(unit)

trait Geysir extends Resource with BlockingTiles

object UnitWrapper {

  private val mappingRules: Map[UnitType, (APIUnit => WrapsUnit, Class[_ <: WrapsUnit])] =
    HashMap(
      UnitType.Resource_Vespene_Geyser -> ((new VespeneGeysir(_), classOf[VespeneGeysir])),
      UnitType.Terran_SCV -> ((new SCV(_), classOf[SCV])),
      UnitType.Terran_Supply_Depot -> ((new SupplyDepot(_), classOf[SupplyDepot])),
      UnitType.Terran_Command_Center -> ((new CommandCenter(_), classOf[CommandCenter])),
      UnitType.Terran_Barracks -> ((new Barracks(_), classOf[Barracks])),
      UnitType.Terran_Academy -> ((new Academy(_), classOf[Academy])),
      UnitType.Terran_Armory -> ((new Academy(_), classOf[Academy])),
      UnitType.Terran_Bunker -> ((new Bunker(_), classOf[Bunker])),
      UnitType.Terran_Comsat_Station -> ((new Comsat(_), classOf[Comsat])),
      UnitType.Terran_Control_Tower -> ((new ControlTower(_), classOf[ControlTower])),
      UnitType.Terran_Engineering_Bay -> ((new EngineeringBay(_), classOf[EngineeringBay])),
      UnitType.Terran_Factory -> ((new Factory(_), classOf[Factory])),
      UnitType.Terran_Machine_Shop -> ((new MachineShop(_), classOf[MachineShop])),
      UnitType.Terran_Missile_Turret -> ((new MissileTurret(_), classOf[MissileTurret])),
      UnitType.Terran_Nuclear_Silo -> ((new NuclearSilo(_), classOf[NuclearSilo])),
      UnitType.Terran_Physics_Lab -> ((new PhysicsLab(_), classOf[PhysicsLab])),
      UnitType.Terran_Refinery -> ((new Refinery(_), classOf[Refinery])),
      UnitType.Terran_Starport -> ((new Starport(_), classOf[Starport])),
      UnitType.Terran_Marine -> ((new Marine(_), classOf[Marine])),

      UnitType.Protoss_Probe -> ((new Probe(_), classOf[Probe])),
      UnitType.Zerg_Drone -> ((new Drone(_), classOf[Drone])),
      UnitType.Unknown -> ((new Irrelevant(_), classOf[Irrelevant]))
    )

  def class2UnitType = {
    mappingRules.map { case (k, (_, c)) =>
      c -> k
    }
  }

  def lift(unit: APIUnit) = {
    trace(s"Detected unit of type ${unit.getType}")
    mappingRules.get(unit.getType) match {
      case Some((mapper, _)) => mapper(unit)
      case None =>
        if (unit.getType.isMineralField) new MineralPatch(unit)
        else new Irrelevant(unit)
    }
  }
}

// this should be a part of bwmirror, but it's missing for some reason
object Dependencies {
  private val builtBy: Map[Class[_ <: WrapsUnit], Class[_ <: WrapsUnit]] = Map(
    classOf[SCV] -> classOf[CommandCenter],
    classOf[Marine] -> classOf[Barracks],
    classOf[Irrelevant] -> classOf[Irrelevant]
  )

  def builderOf(unitClass: Class[_ <: WrapsUnit]) = builtBy(unitClass)
}

object TypeMapping {

  private val class2UnitType: Map[Class[_ <: WrapsUnit], UnitType] = UnitWrapper.class2UnitType

  def unitTypeOf[T <: WrapsUnit](c: Class[_ <: T]) = class2UnitType(c)
}