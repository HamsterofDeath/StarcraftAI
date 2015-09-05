package pony

import bwapi.{Order, Race, TechType, Unit => APIUnit, UnitType, UpgradeType}
import pony.Upgrades.SingleTargetMagicSpell
import pony.brain.{HasUniverse, PriorityChain, UnitWithJob, Universe}

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
  def trackOrder(order: UnitOrder): Unit = {
    history.lastOption.foreach(_.trackOrder_!(order))
  }
  override def onTick(): Unit = {
    super.onTick()
    history += HistoryElement(nativeUnit.getOrder, nativeUnit.getOrderTarget, universe.unitManager.jobOf(this))
    if (history.size > maxHistory) {
      history.remove(0)
    }
  }
  def unitHistory = history.reverse
  case class HistoryElement(order: Order, target: APIUnit, job: UnitWithJob[_ <: WrapsUnit]) {
    private var issuedOrder: UnitOrder = _

    def trackOrder_!(issuedOrder: UnitOrder): Unit = {
      this.issuedOrder = issuedOrder
    }
    override def toString: String = s"$order, $issuedOrder, $target, $job"
  }
}

trait WrapsUnit extends HasNativeSCAttributes with HasUniverse {
  val unitId       = WrapsUnit.nextId
  val nativeUnitId = nativeUnit.getID
  val initialType  = nativeUnit.getType
  private var myUniverse: Universe = _

  override def universe: Universe = myUniverse

  def init_!(universe: Universe): Unit = {
    myUniverse = universe
  }
  def nativeUnit: APIUnit
  def unitIdText = Integer.toString(unitId, 36)
  def mySCRace = {
    val r = nativeUnit.getType.getRace
    if (r == Race.Protoss) Protoss
    else if (r == Race.Terran) Terran
    else if (r == Race.Zerg) Zerg
    else {
      throw new IllegalStateException(s"Check this: $this")
    }
  }
  def isBeingCreated = nativeUnit.getRemainingBuildTime > 0
  def onTick() = {

  }
}

trait Controllable extends WrapsUnit with CanDie

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

trait Building extends BlockingTiles with CanDie {

}

class Upgrade(val nativeType: Either[UpgradeType, TechType]) {
  def mineralPriceForStep(step: Int) =
    nativeType.fold(_.mineralPrice(step), _.mineralPrice())

  def gasPriceForStep(step: Int) =
    nativeType.fold(_.gasPrice(step), _.gasPrice())

  def this(u: UpgradeType) {
    this(Left(u))
  }
  def this(t: TechType) {
    this(Right(t))
  }

  def energyCost = nativeType.fold(_ => throw new UnsupportedOperationException(s"Called on $this"), _.energyUsed())

  override def toString = s"Upgrade: ${nativeType.fold(_.c_str(), _.c_str())}"
}

object Upgrades {
  trait CastOnOrganic extends SingleTargetMagicSpell {
    override def canBeCastOn(m: Mobile) = m.isInstanceOf[Organic]
  }
  trait CastOnMechanic extends SingleTargetMagicSpell {
    override def canBeCastOn(m: Mobile) = m.isInstanceOf[Mechanic]
  }
  trait CastOnAll extends SingleTargetMagicSpell {
    override def canBeCastOn(m: Mobile) = true
  }
  trait CastAtFreeTile extends SinglePointMagicSpell {
    override def canBeCastAt(where: MapTilePosition, by: Mobile) =
      by.universe.mapLayers.reallyFreeBuildingTiles.free(where)
  }
  trait CastOnSelf extends SingleTargetMagicSpell {
    self =>
    override def canBeCastOn(m: Mobile) = throw new RuntimeException(s"Why are you asking this?")
  }

  trait SingleTargetMagicSpell extends Upgrade {
    def asNativeTech = nativeType match {
      case Right(tt) => tt
      case _ => !!!
    }

    def asNativeUpgrade = nativeType match {
      case Left(ut) => ut
      case _ => !!!
    }

    def energyNeeded = energyCost

    def canBeCastOn(m: Mobile): Boolean
  }

  trait SinglePointMagicSpell extends Upgrade {
    def asNativeTech = nativeType match {
      case Right(tt) => tt
      case _ => !!!
    }

    def asNativeUpgrade = nativeType match {
      case Left(ut) => ut
      case _ => !!!
    }

    def energyNeeded = energyCost

    def canBeCastAt(where: MapTilePosition, by: Mobile): Boolean
  }
  trait PermanentSpell extends Upgrade

  object Terran {

    case object WraithEnergy extends Upgrade(UpgradeType.Apollo_Reactor)
    case object ShipArmor extends Upgrade(UpgradeType.Terran_Ship_Plating)
    case object VehicleArmor extends Upgrade(UpgradeType.Terran_Vehicle_Plating)
    case object InfantryArmor extends Upgrade(UpgradeType.Terran_Infantry_Armor)
    case object InfantryCooldown extends Upgrade(TechType.Stim_Packs) with SingleTargetMagicSpell with CastOnSelf
    case object InfantryWeapons extends Upgrade(UpgradeType.Terran_Infantry_Weapons)
    case object VehicleWeapons extends Upgrade(UpgradeType.Terran_Vehicle_Weapons)
    case object ShipWeapons extends Upgrade(UpgradeType.Terran_Ship_Weapons)
    case object MarineRange extends Upgrade(UpgradeType.U_238_Shells)
    case object MedicEnergy extends Upgrade(UpgradeType.Caduceus_Reactor)
    case object MedicFlare extends Upgrade(TechType.Optical_Flare) with SingleTargetMagicSpell with CastOnOrganic
    case object MedicHeal extends Upgrade(TechType.Restoration)
    case object GoliathRange extends Upgrade(UpgradeType.Charon_Boosters)
    case object SpiderMines extends Upgrade(TechType.Spider_Mines) with SinglePointMagicSpell with CastAtFreeTile
    case object Defensematrix extends Upgrade(TechType.Defensive_Matrix) with SingleTargetMagicSpell with CastOnAll
    case object VultureSpeed extends Upgrade(UpgradeType.Ion_Thrusters)
    case object TankSiegeMode extends Upgrade(TechType.Tank_Siege_Mode) with SingleTargetMagicSpell with CastOnSelf
    case object EMP extends Upgrade(TechType.EMP_Shockwave) with SingleTargetMagicSpell with CastOnAll
    case object Irradiate extends Upgrade(TechType.Irradiate) with SingleTargetMagicSpell with CastOnAll
    case object ScienceVesselEnergy extends Upgrade(UpgradeType.Titan_Reactor)
    case object GhostStop extends Upgrade(TechType.Lockdown) with SingleTargetMagicSpell with CastOnMechanic
    case object GhostVisiblityRange extends Upgrade(UpgradeType.Ocular_Implants)
    case object GhostEnergy extends Upgrade(UpgradeType.Moebius_Reactor)
    case object GhostCloak extends Upgrade(TechType.Personnel_Cloaking) with PermanentSpell
    case object WraithCloak extends Upgrade(TechType.Cloaking_Field) with PermanentSpell
    case object CruiserGun extends Upgrade(TechType.Yamato_Gun) with SingleTargetMagicSpell with CastOnAll
    case object CruiserEnergy extends Upgrade(UpgradeType.Colossus_Reactor)

  }
}

trait Upgrader extends Controllable with Building {
  def canUpgrade(upgrade: Upgrade) = race.techTree.canUpgrade(getClass, upgrade)
  def isDoingResearch = currentOrder == Order.ResearchTech || currentOrder == Order.Upgrade
}

trait Addon extends Building {

}

trait UnitFactory extends Building with Controllable {
  def canBuild[T <: Mobile](typeOfUnit: Class[_ <: T]) = {
    race.techTree.canBuild(getClass, typeOfUnit)
  }

  def isProducing = nativeUnit.getOrder == Order.Train || nativeUnit.getRemainingTrainTime > 0
}

trait SupplyProvider extends WrapsUnit {

}

trait ResourceGatherPoint

trait MainBuilding extends Building with UnitFactory with ResourceGatherPoint with SupplyProvider

trait SpellcasterBuilding extends Building with Controllable {

}

trait CanDie extends WrapsUnit {
  private var dead = false
  def isDead = dead

  def notifyDead_!(): Unit = {
    dead = true
  }

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
  def isAttacking = isStartingToAttack || nativeUnit.getAirWeaponCooldown > 0 || nativeUnit.getGroundWeaponCooldown > 0
  def isStartingToAttack = nativeUnit.isStartingAttack
}

trait MobileRangeWeapon extends RangeWeapon with Mobile

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

trait CanBuildAddons extends Building {
  private val myAddonArea = LazyVal.from(Area(area.lowerRight.movedBy(1, -1), Size(2, 2)))
  private var attached    = Option.empty[Addon]
  def positionedNextTo(addon: Addon) = {
    myAddonArea.get.upperLeft == addon.tilePosition
  }
  def addonArea = myAddonArea.get
  def canBuildAddon(addon: Class[_ <: Addon]) = race.techTree.canBuildAddon(getClass, addon)
  def isBuildingAddon = Option(nativeUnit.getAddon).exists(_.getRemainingBuildTime > 0)

  def hasCompleteAddon = Option(nativeUnit.getAddon).exists(_.getRemainingBuildTime == 0)

  def notifyAttach_!(addon: Addon): Unit = {
    trace(s"Addon $addon attached to $this")
    assert(attached.isEmpty)
    attached = Some(addon)
  }
  def notifyDetach_!(addon: Addon): Unit = {
    trace(s"Addon $addon detached from $this")
    assert(attached.isDefined)
    attached = Some(addon)
  }
  def hasAddonAttached = attached.isDefined
}

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
  val blockingAreaForMainBuilding = {
    val ul = area.upperLeft.movedBy(-3, -3)
    val lr = area.lowerRight.movedBy(3, 3)
    Area(ul, lr)
  }

  def remaining = nativeUnit.getResources
}

trait ArmedBuilding extends Building with RangeWeapon

trait ImmobileSupplyProvider extends SupplyProvider with Building
trait MobileSupplyProvider extends SupplyProvider with Mobile

trait GasProvider extends Resource with Building

class MineralPatch(unit: APIUnit) extends AnyUnit(unit) with Resource {
  def isBeingMined = nativeUnit.isBeingGathered
  def remainingMinerals = remaining
}

class VespeneGeysir(unit: APIUnit) extends AnyUnit(unit) with Geysir with Resource {
}

trait SupportUnit extends Mobile

class SupplyDepot(unit: APIUnit) extends AnyUnit(unit) with ImmobileSupplyProvider
class Pylon(unit: APIUnit) extends AnyUnit(unit) with ImmobileSupplyProvider
class Overlord(unit: APIUnit) extends AnyUnit(unit) with MobileSupplyProvider with TransporterUnit with CanDetectHidden

class SCV(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit
class Probe(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit
class Drone(unit: APIUnit) extends AnyUnit(unit) with WorkerUnit

class Shuttle(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit with SupportUnit
class Dropship(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit with SupportUnit

class CommandCenter(unit: APIUnit) extends AnyUnit(unit) with MainBuilding with CanBuildAddons
class Nexus(unit: APIUnit) extends AnyUnit(unit) with MainBuilding
class Hive(unit: APIUnit) extends AnyUnit(unit) with MainBuilding

class Comsat(unit: APIUnit) extends AnyUnit(unit) with SpellcasterBuilding with Addon
class NuclearSilo(unit: APIUnit) extends AnyUnit(unit) with SpellcasterBuilding with Addon
class PhysicsLab(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon
class Refinery(unit: APIUnit) extends AnyUnit(unit) with GasProvider
class CovertOps(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon
class MachineShop(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon
class ControlTower(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon
class RocketTower(unit: APIUnit) extends AnyUnit(unit) with ArmedBuilding

class Barracks(unit: APIUnit) extends AnyUnit(unit) with UnitFactory
class Factory(unit: APIUnit) extends AnyUnit(unit) with UnitFactory with CanBuildAddons
class Starport(unit: APIUnit) extends AnyUnit(unit) with UnitFactory with CanBuildAddons

class Academy(unit: APIUnit) extends AnyUnit(unit) with Upgrader
class Armory(unit: APIUnit) extends AnyUnit(unit) with Upgrader
class EngineeringBay(unit: APIUnit) extends AnyUnit(unit) with Upgrader
class ScienceFacility(unit: APIUnit) extends AnyUnit(unit) with Upgrader with CanBuildAddons

class MissileTurret(unit: APIUnit) extends AnyUnit(unit) with ArmedBuilding with CanDetectHidden

class Bunker(unit: APIUnit) extends AnyUnit(unit)

trait InstantAttack extends Mobile
trait MobileDetector extends CanDetectHidden with Mobile
trait CanDetectHidden extends WrapsUnit
trait CanUseStimpack extends Mobile with Weapon {
  def isStimmed = nativeUnit.isStimmed || stimTime > 0
  def stimTime = nativeUnit.getStimTimer
}
trait PermaCloak extends CanCloak {
  override def isCloaked = true
}
trait CanCloak extends Mobile {
  def isCloaked = nativeUnit.isCloaked
}

abstract class SingleTargetSpell[C <: HasSingleTargetSpells](tech: Upgrade with SingleTargetMagicSpell) {

}

object Spells {
  case object Lockdown extends SingleTargetSpell[Ghost](Upgrades.Terran.GhostStop)
  case object Blind extends SingleTargetSpell[Medic](Upgrades.Terran.MedicFlare)
}

trait HasMana extends WrapsUnit {
  def mana = nativeUnit.getEnergy
}

trait HasSingleTargetSpells extends Mobile with HasMana {
  type MyType <: HasSingleTargetSpells
  val spells: Seq[SingleTargetSpell[MyType]]
  private   var lastCast = 0
  protected val cooldown = 24

  def canCastNow(tech: SingleTargetMagicSpell) = {
    def hasMana = tech.energyNeeded <= mana
    def isReadyForCastCool = lastCast + cooldown < universe.currentTick
    hasMana && isReadyForCastCool
  }

  def toOrder(tech: SingleTargetMagicSpell, target: Mobile) = {
    assert(canCastNow(tech))
    assert(tech.canBeCastOn(target))
    lastCast = universe.currentTick
    Orders.TechOnTarget(this, target, tech)
  }
}

trait Mechanic extends Mobile
trait Organic extends Mobile

class Observer(unit: APIUnit) extends AnyUnit(unit) with MobileDetector with Mechanic
class Scout(unit: APIUnit) extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic
class Zealot(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon
class Dragoon(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with Mechanic
class Archon(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon
class Carrier(unit: APIUnit) extends AnyUnit(unit) with AirUnit with Mechanic
class Arbiter(unit: APIUnit) extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic
class Templar(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with PermaCloak with HasSingleTargetSpells {
  override val spells = Nil
}
class DarkTemplar(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon with CanCloak
class DarkArchon(unit: APIUnit) extends AnyUnit(unit) with GroundUnit
class Corsair(unit: APIUnit) extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic
class Interceptor(unit: APIUnit) extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic
class Reaver(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon with Mechanic

class Scarab(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon
class SpiderMine(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon

class Marine(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanUseStimpack
class Firebat(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon with CanUseStimpack
class Ghost(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanCloak with InstantAttack with
          HasSingleTargetSpells {
  override val spells = List(Spells.Lockdown)
  override type MyType = Ghost
}
class Medic(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with SupportUnit with HasSingleTargetSpells {
  override val spells = List(Spells.Blind)
  override type MyType = Medic
}
class Vulture(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with SpiderMines with InstantAttack with Mechanic
class Tank(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with GroundWeapon with InstantAttack with Mechanic
class Goliath(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with InstantAttack with Mechanic
class Wraith(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with CanCloak with InstantAttack with Mechanic
class Valkery(unit: APIUnit) extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic
class Battlecruiser(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with InstantAttack with Mechanic with
          HasSingleTargetSpells {
  override val spells = Nil
}
class ScienceVessel(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with SupportUnit with CanDetectHidden with Mechanic with HasSingleTargetSpells {
  override val spells = Nil
}

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
      UnitType.Terran_Armory -> ((new Armory(_), classOf[Armory])),
      UnitType.Terran_Science_Facility -> ((new ScienceFacility(_), classOf[ScienceFacility])),
      UnitType.Terran_Bunker -> ((new Bunker(_), classOf[Bunker])),
      UnitType.Terran_Firebat -> ((new Firebat(_), classOf[Firebat])),
      UnitType.Terran_Comsat_Station -> ((new Comsat(_), classOf[Comsat])),
      UnitType.Terran_Covert_Ops -> ((new CovertOps(_), classOf[CovertOps])),
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
      UnitType.Terran_Medic -> ((new Medic(_), classOf[Medic])),
      UnitType.Terran_Valkyrie -> ((new Valkery(_), classOf[Valkery])),
      UnitType.Terran_Vulture -> ((new Vulture(_), classOf[Vulture])),
      UnitType.Terran_Siege_Tank_Tank_Mode -> ((new Tank(_), classOf[Tank])),
      UnitType.Terran_Siege_Tank_Siege_Mode -> ((new Tank(_), classOf[Tank])),
      UnitType.Terran_Goliath -> ((new Goliath(_), classOf[Goliath])),
      UnitType.Terran_Wraith -> ((new Wraith(_), classOf[Wraith])),
      UnitType.Terran_Science_Vessel -> ((new ScienceVessel(_), classOf[ScienceVessel])),
      UnitType.Terran_Battlecruiser -> ((new Battlecruiser(_), classOf[Battlecruiser])),
      UnitType.Terran_Dropship -> ((new Dropship(_), classOf[Dropship])),
      UnitType.Terran_Ghost -> ((new Ghost(_), classOf[Ghost])),
      UnitType.Terran_Vulture_Spider_Mine -> ((new SpiderMine(_), classOf[SpiderMine])),

      UnitType.Protoss_Probe -> ((new Probe(_), classOf[Probe])),
      UnitType.Protoss_Zealot -> ((new Zealot(_), classOf[Zealot])),
      UnitType.Protoss_Dragoon -> ((new Dragoon(_), classOf[Dragoon])),
      UnitType.Protoss_Archon -> ((new Archon(_), classOf[Archon])),
      UnitType.Protoss_Dark_Archon -> ((new DarkArchon(_), classOf[DarkArchon])),
      UnitType.Protoss_Dark_Templar -> ((new DarkTemplar(_), classOf[DarkTemplar])),
      UnitType.Protoss_Reaver -> ((new Reaver(_), classOf[Reaver])),
      UnitType.Protoss_High_Templar -> ((new Templar(_), classOf[Templar])),
      UnitType.Protoss_Corsair -> ((new Corsair(_), classOf[Corsair])),
      UnitType.Protoss_Interceptor -> ((new Interceptor(_), classOf[Interceptor])),
      UnitType.Protoss_Observer -> ((new Observer(_), classOf[Observer])),
      UnitType.Protoss_Scarab -> ((new Scarab(_), classOf[Scarab])),
      UnitType.Protoss_Scout -> ((new Scout(_), classOf[Scout])),

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

object TypeMapping {

  private val class2UnitType: Map[Class[_ <: WrapsUnit], UnitType] = UnitWrapper.class2UnitType

  // TODO return cached copies to save native calls
  def unitTypeOf[T <: WrapsUnit](c: Class[_ <: T]) = class2UnitType(c)
}