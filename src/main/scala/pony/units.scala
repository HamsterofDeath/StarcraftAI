package pony

import bwapi.{Order, Race, TechType, Unit => APIUnit, UnitType, UpgradeType, WeaponType}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait NiceToString extends WrapsUnit {
  override def toString = s"[$unitIdText] ${getClass.className}"
}

abstract class AnyUnit(val nativeUnit: APIUnit)
  extends WrapsUnit with NiceToString with OrderHistorySupport {

}

trait OrderHistorySupport extends WrapsUnit {
  private val history    = ListBuffer.empty[HistoryElement]
  private val maxHistory = if (memoryHog) 1000 else 24
  def trackOrder(order: UnitOrder): Unit = {
    history.lastOption.foreach(_.trackOrder_!(order))
  }
  override def onTick_!(): Unit = {
    super.onTick_!()
    if (universe.world.debugger.isDebugging) {
      if (universe.unitManager.hasJob(this)) {
        history += HistoryElement(nativeUnit.getOrder, nativeUnit.getOrderTarget,
          universe.unitManager.jobOf(this))
        if (history.size > maxHistory) {
          history.remove(0)
        }
      }
    }
  }

  def unitHistory = history.reverseIterator

  case class HistoryElement(order: Order, target: APIUnit, job: UnitWithJob[_ <: WrapsUnit]) {
    private var issuedOrder: UnitOrder = _

    def trackOrder_!(issuedOrder: UnitOrder): Unit = {
      this.issuedOrder = issuedOrder
    }

    override def toString: String = s"$order, $issuedOrder, $target, $job"
  }

}

trait WrapsUnit extends HasUniverse with AfterTickListener {

  val unitId            = WrapsUnit.nextId
  val nativeUnitId      = nativeUnit.getID
  val initialNativeType = nativeUnit.getType
  val nativeType        = oncePerTick {
    val ret = nativeUnit.getType
    morphed |= ret != initialNativeType
    ret
  }
  private val nativeOrder          = oncePerTick {
    nativeUnit.getOrder
  }
  private val curOrder             = oncePerTick {nativeUnit.getOrder}
  private val unfinished           = oncePerTick(
    nativeUnit.getRemainingBuildTime > 0 || !nativeUnit.isCompleted)
  private val myCenterTile         = oncePerTick {
    val c = center
    MapTilePosition(c.x / 32, c.y / 32)
  }
  private var morphed              = false
  private var inGame               = true
  private var creationTick         = -1
  private var myUniverse: Universe = _

  def hasEverMorphed = morphed

  def currentNativeOrder = nativeOrder.get

  def hasSpells = false

  def notifyRemoved_!(): Unit = {
    inGame = false
    universe.unregister_!(this)
  }

  override def universe: Universe = {
    assert(myUniverse != null)
    myUniverse
  }

  def isDoingNothing = {
    val order = nativeOrder.get
    order == Order.PlayerGuard || order == Order.Nothing
  }

  override def postTick(): Unit = {

  }

  def onMorph(getType: UnitType) = {

  }

  def shouldReRegisterOnMorph: Boolean

  def currentOrder = curOrder.get

  def age = universe.currentTick - creationTick

  def isInGame = {
    val ct = centerTile
    inGame && ct.x < 1000 && ct.y < 1000
  }

  def centerTile = myCenterTile.get

  def canDoDamage = false

  def init_!(universe: Universe): Unit = {
    myUniverse = universe
    onUniverseSet(universe)
    creationTick = universe.currentTick
    universe.register_!(this)
  }

  protected def onUniverseSet(universe: Universe): Unit = {

  }

  def center: MapPosition

  def isSelected = nativeUnit.isSelected
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
  def isBeingCreated = unfinished.get
  override def onTick_!() = {
    super.onTick_!()
  }

  object surroundings {
    private val myEnemyGroundUnits = oncePer(Primes.prime23) {
      universe.unitGrid.enemy.allInRange[GroundUnit](centerTile, 12).toVector
    }

    private val myCloseOwnBuildings = oncePer(Primes.prime29) {
      ownUnits.allBuildings.filter(_.centerTile.distanceToIsLess(centerTile, 12))
    }

    private val myCloseEnemyBuildings = oncePer(Primes.prime29) {
      enemies.allBuildings.filter(_.centerTile.distanceToIsLess(centerTile, 12))
    }

    def closeEnemyBuildings = myCloseEnemyBuildings.get

    def closeEnemyGroundUnits = myEnemyGroundUnits.get

    def closeOwnBuildings = myCloseOwnBuildings.get

  }

}

trait Controllable extends WrapsUnit with MaybeCanDie

object WrapsUnit {
  private var counter = 0

  def nextId = {
    val ret = counter
    counter += 1
    ret
  }
}

trait StaticallyPositioned extends WrapsUnit {

  self =>

  val myTilePosition = once {
    val position = nativeUnit.getTilePosition
    val x = position.getX
    val y = position.getY
    MapTilePosition.shared(x, y)
  }

  val size = Size.shared(nativeUnit.getType.tileWidth(), nativeUnit.getType.tileHeight())

  private val myArea = once {
    Area(tilePosition, size)
  }

  private val myAreaOnMap = once {
    mapLayers.rawWalkableMap.areaOf(centerTile)
    .getOr(s"Building is not on valid ground: $self")
  }

  def areaOnMap = myAreaOnMap.get

  override def shouldReRegisterOnMorph = true

  def nativeMapPosition = tilePosition.asMapPosition.toNative

  def tilePosition = myTilePosition.get

  override def center = area.center

  def area = myArea.get

  override def toString: String = {
    s"${super.toString}@${area.describe}"
  }
}

trait BlockingTiles extends StaticallyPositioned {

}

trait IsInfantry extends WrapsUnit with BadDancer

trait IsVehicle extends WrapsUnit

trait IsShip extends WrapsUnit

trait TerranBuilding extends Building {
  private val myCurrentArea = oncePer(Primes.prime59) {
    mapLayers.rawWalkableMap.areaOf(centerTile).orElse {
      mapLayers.rawWalkableMap
      .spiralAround(centerTile, 5)
      .map(mapLayers.rawWalkableMap.areaOf)
      .find(_.isDefined)
      .map(_.get)
    }
  }

  def currentAreaOnMap = myCurrentArea.get

}

trait Building extends BlockingTiles with MaybeCanDie {
  self =>
  override val armorType            = Building
  private  val myFlying             = oncePerTick {
    nativeUnit.isFlying
  }
  private  val myAbandoned          = oncePerTick {
    isBeingCreated && incomplete && !isInstanceOf[Addon] && {
      val myClass = getClass
      val takenCareOf = unitManager.constructionsInProgress(myClass).exists { job =>
        job.building.contains(self)
      }
      !takenCareOf
    }

  }
  private  val myRemainingBuildTime = oncePerTick {
    nativeUnit.getRemainingBuildTime
  }

  def isFloating = myFlying.get

  def incomplete = currentNativeOrder == Order.IncompleteBuilding || remainingBuildTime > 0

  def remainingBuildTime = myRemainingBuildTime.get

  def isIncompleteAbandoned = myAbandoned.get
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

  def energyCost = nativeType.fold(_ => throw new UnsupportedOperationException(s"Called on $this"),
    _.energyCost())

  override def toString = s"Upgrade: ${nativeType.fold(_.toString, _.toString)}"

  override def equals(other: Any): Boolean = other match {
    case that: Upgrade =>
      nativeType == that.nativeType
    case _ => false
  }

  override def hashCode(): Int = {
    nativeType.hashCode()
  }
}

object Upgrades {

  def allTech = Terran.allTech

  trait CastOnOrganic extends SingleTargetMagicSpell {
    override val canCastOn = classOf[Organic]
  }

  trait CastOnMechanic extends SingleTargetMagicSpell {
    override val canCastOn = classOf[Mechanic]
  }

  trait CastOnAll extends SingleTargetMagicSpell {
    override val canCastOn = classOf[Mobile]
  }

  trait CastAtFreeTile extends SinglePointMagicSpell {
    override def canBeCastAt(where: MapTilePosition, by: Mobile) =
      by.universe.mapLayers.freeWalkableTiles.free(where)
  }

  trait CastOnSelf extends SingleTargetMagicSpell {
    self =>
  }

  trait SingleTargetMagicSpell extends Upgrade with IsTech {
    val canCastOn: Class[_ <: Mobile]

    def asNativeTech = nativeType match {
      case Right(tt) => tt
      case _ => !!!
    }

    def asNativeUpgrade = nativeType match {
      case Left(ut) => ut
      case _ => !!!
    }

    def energyNeeded = energyCost

  }

  trait SinglePointMagicSpell extends Upgrade with IsTech {
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

  trait IsTech extends Upgrade {
    // calling this is an assertion
    nativeTech

    def nativeTech = nativeType match {
      case Left(_) => !!!
      case Right(t) => t
    }

    def priorityRule: Option[Mobile => Double] = None
  }

  object Protoss {

    case object InfantryArmor extends Upgrade(UpgradeType.None)

    case object VehicleArmor extends Upgrade(UpgradeType.None)

    case object ShipArmor extends Upgrade(UpgradeType.None)

  }

  object Zerg {

    case object InfantryArmor extends Upgrade(UpgradeType.None)

    case object VehicleArmor extends Upgrade(UpgradeType.None)

    case object ShipArmor extends Upgrade(UpgradeType.None)

  }

  object Fake {

    case object BuildingArmor extends Upgrade(UpgradeType.None)

  }

  object Terran {

    val allTech = InfantryCooldown :: MedicFlare :: MedicHeal :: SpiderMines :: Defensematrix ::
                  TankSiegeMode :: EMP :: Irradiate :: GhostStop :: GhostCloak :: WraithCloak ::
                  CruiserGun :: Nil

    case object WraithEnergy extends Upgrade(UpgradeType.Apollo_Reactor)

    case object ShipArmor extends Upgrade(UpgradeType.Terran_Ship_Plating)

    case object VehicleArmor extends Upgrade(UpgradeType.Terran_Vehicle_Plating)

    case object InfantryArmor extends Upgrade(UpgradeType.Terran_Infantry_Armor)

    case object InfantryCooldown
      extends Upgrade(TechType.Stim_Packs) with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanUseStimpack]
    }

    case object InfantryWeapons extends Upgrade(UpgradeType.Terran_Infantry_Weapons)

    case object VehicleWeapons extends Upgrade(UpgradeType.Terran_Vehicle_Weapons)

    case object ShipWeapons extends Upgrade(UpgradeType.Terran_Ship_Weapons)

    case object MarineRange extends Upgrade(UpgradeType.U_238_Shells)

    case object MedicEnergy extends Upgrade(UpgradeType.Caduceus_Reactor)

    case object MedicFlare
      extends Upgrade(TechType.Optical_Flare) with SingleTargetMagicSpell with CastOnOrganic with
              DetectorsFirst

    case object MedicHeal
      extends Upgrade(TechType.Restoration) with SingleTargetMagicSpell with CastOnAll

    case object GoliathRange extends Upgrade(UpgradeType.Charon_Boosters)

    case object SpiderMines
      extends Upgrade(TechType.Spider_Mines) with SinglePointMagicSpell with CastAtFreeTile

    case object ScannerSweep
      extends Upgrade(TechType.Scanner_Sweep) with SinglePointMagicSpell with CastAtFreeTile

    case object Nuke
      extends Upgrade(TechType.Nuclear_Strike) with SinglePointMagicSpell with CastAtFreeTile

    case object Defensematrix
      extends Upgrade(TechType.Defensive_Matrix) with SingleTargetMagicSpell with CastOnAll

    case object VultureSpeed extends Upgrade(UpgradeType.Ion_Thrusters)

    case object TankSiegeMode
      extends Upgrade(TechType.Tank_Siege_Mode) with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanSiege]
    }

    case object EMP
      extends Upgrade(TechType.EMP_Shockwave) with SingleTargetMagicSpell with CastOnAll

    case object Irradiate
      extends Upgrade(TechType.Irradiate) with SingleTargetMagicSpell with CastOnAll

    case object ScienceVesselEnergy extends Upgrade(UpgradeType.Titan_Reactor)

    case object GhostStop
      extends Upgrade(TechType.Lockdown) with SingleTargetMagicSpell with CastOnMechanic with
              ByPrice

    case object GhostVisiblityRange extends Upgrade(UpgradeType.Ocular_Implants)

    case object GhostEnergy extends Upgrade(UpgradeType.Moebius_Reactor)

    case object GhostCloak
      extends Upgrade(TechType.Personnel_Cloaking) with PermanentSpell with
              SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanCloak]
    }

    case object WraithCloak
      extends Upgrade(TechType.Cloaking_Field) with PermanentSpell with SingleTargetMagicSpell with
              CastOnSelf {
      override val canCastOn = classOf[CanCloak]
    }

    case object CruiserGun
      extends Upgrade(TechType.Yamato_Gun) with SingleTargetMagicSpell with CastOnAll

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

trait AreaSpellcasterBuilding
  extends Building with Controllable with HasSinglePointMagicSpell with HasMana {

  override def canCastNow(tech: SinglePointMagicSpell) = {
    def hasMana = tech.energyNeeded <= mana
    super.canCastNow(tech) && hasMana
  }
}

case class HitPoints(hitpoints: Int, shield: Int) {

  def isDead = hitpoints == 0

  def sum = hitpoints + shield

  def <=(other: HitPoints): Boolean = <=(other.hitpoints, other.shield)

  def <=(otherHp: Int, otherShield: Int) = {
    hitpoints <= otherHp || shield <= otherShield
  }

  def <(other: HitPoints): Boolean = <(other.hitpoints, other.shield)

  def <(otherHp: Int, otherShield: Int) = {
    hitpoints < otherHp || shield < otherShield
  }

  def <(t: (Int, Int)) = {
    hitpoints < t._1 || shield < t._2
  }
}

sealed class DamageFactor(val factor: Int)

case object Full extends DamageFactor(100)

case object ThreeQuarters extends DamageFactor(75)

case object Half extends DamageFactor(50)

case object Quarter extends DamageFactor(25)

case object Zero extends DamageFactor(0)

sealed class DamageType(native: bwapi.DamageType)

case object Normal extends DamageType(bwapi.DamageType.Normal)

case object Explosive extends DamageType(bwapi.DamageType.Explosive)

case object Concussive extends DamageType(bwapi.DamageType.Concussive)

case object IgnoreArmor extends DamageType(bwapi.DamageType.Ignore_Armor)

case object Independent extends DamageType(bwapi.DamageType.Independent)

case object Unknown extends DamageType(bwapi.DamageType.Unknown)

case object NoDamage extends DamageType(bwapi.DamageType.None)

object DamageTypes {
  def fromNative(dt: bwapi.DamageType) = {
    if (dt == bwapi.DamageType.Concussive) Concussive
    else if (dt == bwapi.DamageType.Explosive) Explosive
    else if (dt == bwapi.DamageType.Normal) Normal
    else if (dt == bwapi.DamageType.Ignore_Armor) IgnoreArmor
    else if (dt == bwapi.DamageType.Independent) Independent
    else if (dt == bwapi.DamageType.None) NoDamage
    else if (dt == bwapi.DamageType.Unknown) Unknown
    else if (dt eq null) !!!("Null?")
    else !!!(s"Unknown damage type :(")
  }
}

sealed trait ArmorType {
  val tileSize: Size
  def transportSize: Int
  def damageFactorIfHitBy(damageType: DamageType): DamageFactor
}

case object Small extends ArmorType {
  override val tileSize = Size(1, 1)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal => Full
      case Concussive => Full
      case Explosive => Half
      case _ => !!!(s"Check $damageType")
    }
  }

  override def transportSize = 1
}

case object Medium extends ArmorType {
  override val tileSize = Size(1, 1)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal => Full
      case Concussive => Half
      case Explosive => ThreeQuarters
      case _ => !!!(s"Check $damageType")
    }
  }

  override def transportSize = 2
}

case object Indestructible extends ArmorType {
  override val tileSize = Size(1, 1)

  override def damageFactorIfHitBy(damageType: DamageType) = Zero

  override def transportSize = !!!("This should never happen")
}

case object Large extends ArmorType {
  override val tileSize = Size(2, 2)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal => Full
      case Concussive => Quarter
      case Explosive => Full
      case _ => !!!(s"Check $damageType")
    }
  }

  override def transportSize = 4
}

case object Building extends ArmorType {
  override val tileSize = Size(4, 3)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal => Full
      case Concussive => Quarter
      case Explosive => Full
      case _ => !!!(s"Check $damageType")
    }
  }

  override def transportSize = !!!("This should never happen")
}

case class Armor(armorType: ArmorType, hp: HitPoints, armor: Int, owner: MaybeCanDie)

trait MaybeCanDie extends WrapsUnit {
  self =>

  val armorType: ArmorType
  val price = Price(nativeUnit.getType.mineralPrice(), nativeUnit.getType.gasPrice())
  private val maxHp       = nativeUnit.getType.maxHitPoints()
  private val maxShields  = nativeUnit.getType.maxShields()
  private val disabled    = oncePerTick {evalLocked}
  private val myArmor     = oncePerTick {
    val hp = HitPoints(nativeUnit.getHitPoints, nativeUnit.getShields)
    val armorLevel = universe.upgrades.armorForUnitType(self)
    Armor(armorType, hp, armorLevel, self)
  }
  private var lastFrameHp = HitPoints(-1, -1)
  // obviously wrong, but that doesn't matter
  private var dead        = false
  def percentageHPOk = {
    hitPoints.sum.toDouble / (maxHp + maxShields)
  }

  def isDamaged = isInGame && (hitPoints.shield < maxShields || hitPoints.hitpoints < maxHp) &&
                  !isBeingCreated

  def hitPoints = currentHp

  private def currentHp = myArmor.get.hp

  override def isInGame: Boolean = super.isInGame && !isDead

  def isDead = dead || currentHp.isDead

  def isNonFighter = isUnArmed || isInstanceOf[WorkerUnit]

  def isUnArmed = !canDoDamage && !hasSpells

  def isHarmlessNow = isIncapacitated || isUnArmed

  def isIncapacitated = disabled.get

  def isBeingAttacked = currentHp < lastFrameHp

  def notifyDead_!(): Unit = {
    dead = true
  }

  def matchThis[X](ifMobile: Mobile => X, ifBuilding: Building => X) = this match {
    case m: Mobile => ifMobile(m)
    case b: Building => ifBuilding(b)
    case x => !!!(s"Check this $x")
  }

  def armor = myArmor.get

  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.register_!(() => {
      lastFrameHp = currentHp
    })
  }

  private def evalLocked = nativeUnit.isLockedDown || nativeUnit.isStasised
}

object Price {
  val zero = Price(0, 0)
  implicit val ord = Ordering.fromLessThan[Price](_ < _)
}

case class Price(minerals: Int, gas: Int) {
  val sum = minerals + gas

  def -(other: Price) = Price(minerals - other.minerals, gas - other.gas)

  def <(other: Price) = sum < other.sum

  def +(price: Price) = Price(minerals + price.minerals, gas + price.gas)
}

trait IndestructibleUnit extends WrapsUnit {

}

trait AutoPilot extends Mobile {
  def isManuallyControlled = !isAutoPilot
  override def isAutoPilot = true
}

trait Mobile extends WrapsUnit with Controllable {

  def isGroundUnit: Boolean
  def asGroundUnit = if (isGroundUnit) this.asInstanceOf[GroundUnit].toSome else None
  def canSee(tile: MapTilePosition) = mapLayers.rawWalkableMap.connectedByLine(tile, currentTile)
  val buildPrice = Price(nativeUnit.getType.mineralPrice(), nativeUnit.getType.gasPrice())
  private val myCurrentArea         = oncePer(Primes.prime11) {
    mapLayers.rawWalkableMap.areaOf(currentTile).orElse {
      mapLayers.rawWalkableMap
      .spiralAround(currentTile, 2)
      .map(mapLayers.rawWalkableMap.areaOf)
      .find(_.isDefined)
      .map(_.get)
    }
  }
  private val defenseMatrix         = oncePerTick {
    nativeUnit.getDefenseMatrixPoints > 0 || nativeUnit.getDefenseMatrixTimer > 0
  }
  private val defenseMatrixHP       = oncePerTick {
    nativeUnit.getDefenseMatrixPoints
  }
  private val irradiation           = oncePerTick {
    nativeUnit.getIrradiateTimer > 0
  }
  private val myTile                = oncePerTick {
    val tp = nativeUnit.getPosition
    MapTilePosition.shared(tp.getX / 32, tp.getY / 32)
  }
  private val myArea                = oncePerTick {
    Area(myTile.get, armorType.tileSize)
  }
  private val myCurrentPosition     = oncePerTick {
    val p = nativeUnit.getPosition
    MapPosition(p.getX, p.getY)
  }
  private var lastFrameMatrixPoints = 0
  def canMove = true
  def currentArea = myCurrentArea.get
  override def shouldReRegisterOnMorph = false
  def isAutoPilot = false
  override def isBeingAttacked: Boolean = super.isBeingAttacked || matrixHp < lastFrameMatrixPoints
  def matrixHp = defenseMatrixHP.get
  def hasDefenseMatrix = defenseMatrix.get
  def isIrradiated = irradiation.get
  override def center = currentPosition

  def isGuarding = nativeUnit.getOrder == Order.PlayerGuard

  def isMoving = nativeUnit.isMoving

  def currentTileNative = currentTile.asNative

  def currentTile = myTile.get

  def blockedArea = myArea.get
  def unitTileSize = armor.armorType.tileSize
  def currentPositionNative = currentPosition.toNative

  def currentPosition = myCurrentPosition.get

  override def toString = s"${super.toString}@$currentTile"

  override def onTick_!(): Unit = {
    super.onTick_!()
    defenseMatrix.invalidate()
  }
  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.register_!(() => {
      lastFrameMatrixPoints = nativeUnit.getDefenseMatrixPoints
    })
  }
}

trait Killable {

}

case class Circle(center: MapTilePosition, radius: Int, maxX: Int, maxY: Int) {
  def asTiles = new GeometryHelpers(maxX, maxY).tilesInCircle(center, radius)
}

trait DetectorBuilding extends CanDetectHidden with Building

trait Detector extends WrapsUnit with HasLazyVals {
  private val myDetectionArea = oncePerTick {
    geoHelper.circle(centerTile, detectionRadius)
  }

  def detectionRadius: Int

  def detectionArea = myDetectionArea.get
}

trait ArmedUnit extends WrapsUnit {

}

trait HasSpiderMines extends WrapsUnit {
  private val mines = oncePerTick {nativeUnit.getSpiderMineCount}
  def spiderMineCount = mines.get
}

trait GroundWeapon extends Weapon {
  val groundRange            = groundWeapon.maxRange()
  val groundCanAttackAir     = groundWeapon.targetsAir()
  val groundCanAttackGround  = groundWeapon.targetsGround()
  val groundDamageMultiplier = groundWeapon.damageFactor()
  val groundDamageType: DamageType
  protected lazy val groundWeapon          = initialNativeType.groundWeapon()
  private        val damage                = LazyVal.from {
    // will be invalidated on upgrade
    evalDamage(groundWeapon, groundDamageType, groundDamageMultiplier, targetsAir = false)
  }
  private        val myInGroundWeaponRange = oncePerTick {
    geoHelper.circle(centerTile, math.round(groundRange.toDouble / 32).toInt)
  }

  def damageDelayFactorGround: Int

  override def weaponRangeRadius: Int = super.weaponRangeRadius max groundRange

  override def assumeShotDelayOn(target: MaybeCanDie) = {
    if (canAttack(target)) {
      damageDelayFactorGround
    } else
      super.assumeShotDelayOn(target)
  }

  override def canAttack(other: MaybeCanDie) = {
    super.canAttack(other) || selfCanAttack(other)
  }

  def inGroundWeaponRange = myInGroundWeaponRange.get

  override def calculateDamageOn(other: Armor, assumeHP: Int, assumeShields: Int,
                                 shotCount: Int) = {
    if (selfCanAttack(other.owner)) {
      damage.get.damageIfHits(other, assumeHP, assumeShields, shotCount)
    } else {
      super.calculateDamageOn(other, assumeHP, assumeShields, shotCount)
    }
  }

  private def selfCanAttack(other: MaybeCanDie) = {
    matchOn[Boolean](other)(
      _ => groundCanAttackAir,
      _ => groundCanAttackGround,
      b => if (b.isFloating) groundCanAttackAir else groundCanAttackGround)
  }

  override def isInWeaponRange(other: MaybeCanDie) = {
    if (selfCanAttack(other))
    // TODO use own logic here
      matchOn(other)(air => nativeUnit.isInWeaponRange(other.nativeUnit),
        ground => nativeUnit.isInWeaponRange(other.nativeUnit),
        building => nativeUnit.isInWeaponRange(other.nativeUnit))
    else
      super.isInWeaponRange(other)
  }
  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.upgrades.register_!(_ => damage.invalidate())
  }
}

case class Damage(baseAmount: Int, bonus: Int, cooldown: Int, damageType: DamageType, hitCount: Int,
                  upgradeLevel: Int, isAir: Boolean) {
  def damageIfHits(other: Armor, assumeHP: Int, assumeShields: Int, shotCount: Int) = {
    val maxDamage = upgradeLevel * bonus + baseAmount
    // TODO exact damage calculation is still unknown

    def calculate(against: Armor, assumeHP: Int, assumeShields: Int) = {
      val shield = assumeShields
      val subtractFromShields = {
        if (shield > 0) {
          assumeHP min maxDamage
        } else 0
      }

      val damageToHP = {
        val remainingDamage = maxDamage - subtractFromShields
        if (remainingDamage > 0) {
          val armorDivisor = against.armorType.damageFactorIfHitBy(damageType)
          val armor = against.armor
          val afterArmor = (remainingDamage - armor) max 1
          val afterFactor = (afterArmor * armorDivisor.factor) / 100
          afterFactor
        } else 0
      }
      DamageSingleAttack(damageToHP, subtractFromShields, isAir)
    }

    var shots = shotCount
    var hp = assumeHP
    var shields = assumeShields
    var hpDamage = 0
    var shieldDamage = 0
    while (shots > 0) {
      shots -= 1

      val damageOfFirstHit = calculate(other, hp, shields)
      val afterShot = {
        if (hitCount == 2) {
          val hpAfterFirstHit = hp - damageOfFirstHit.onHp
          val shieldAfterFirstHit = shields - damageOfFirstHit.onShields
          val damageOfSecondHit = calculate(other, hpAfterFirstHit, shieldAfterFirstHit)
          DamageSingleAttack(damageOfFirstHit.onHp + damageOfSecondHit.onHp,
            damageOfFirstHit.onShields + damageOfSecondHit.onShields, isAir)
        } else {
          assert(hitCount == 1)
          damageOfFirstHit
        }
      }

      hp -= afterShot.onHp
      shields -= afterShot.onShields
      hpDamage += afterShot.onHp
      shieldDamage += afterShot.onShields

    }

    DamageSingleAttack(hpDamage, shieldDamage, isAir)
  }
}

trait HasHpAndShields {
  val hp     : Int
  val shields: Int
}

case class DamageSingleAttack(onHp: Int, onShields: Int, airHit: Boolean) extends HasHpAndShields {
  override val hp     : Int = onHp
  override val shields: Int = onShields
}

trait AutoAirWeaponType extends AirWeapon {
  override val airDamageType = DamageTypes.fromNative(airWeapon.damageType)
}

trait AutoGroundWeaponType extends GroundWeapon {
  override val groundDamageType = DamageTypes.fromNative(groundWeapon.damageType)
}

trait AirWeapon extends Weapon {
  val airRange            = airWeapon.maxRange()
  val airCanAttackAir     = airWeapon.targetsAir()
  val airCanAttackGround  = airWeapon.targetsGround()
  val airDamageMultiplier = airWeapon.damageFactor()
  val airDamageType: DamageType
  protected lazy val airWeapon = initialNativeType.airWeapon()
  private        val damage    = LazyVal.from {
    // will be invalidated on upgrade
    evalDamage(airWeapon, airDamageType, airDamageMultiplier, targetsAir = true)
  }

  private val myInAirWeaponRange = oncePerTick {
    geoHelper.circle(centerTile, math.round(airRange.toDouble / 32).toInt)
  }
  def inAirWeaponRange = myInAirWeaponRange.get

  def damageDelayFactorAir: Int
  override def weaponRangeRadius: Int = super.weaponRangeRadius max airRange
  override def assumeShotDelayOn(target: MaybeCanDie) = {
    if (canAttack(target)) {
      damageDelayFactorAir
    } else
      super.assumeShotDelayOn(target)
  }
  // air & groundweapon need to override this
  override def canAttack(other: MaybeCanDie) = {
    super.canAttack(other) || selfCanAttack(other)
  }
  override def calculateDamageOn(other: Armor, assumeHP: Int, assumeShields: Int,
                                 shotCount: Int) = {
    if (selfCanAttack(other.owner)) {
      damage.get.damageIfHits(other, assumeHP, assumeShields, shotCount)
    } else {
      super.calculateDamageOn(other, assumeHP, assumeShields, shotCount)
    }
  }

  private def selfCanAttack(other: MaybeCanDie) = {
    matchOn(other)(
      _ => airCanAttackAir,
      _ => airCanAttackGround,
      b => if (b.isFloating) airCanAttackAir else airCanAttackGround)
  }

  override def isInWeaponRange(other: MaybeCanDie) = {
    if (selfCanAttack(other))
    // TODO use own logic
      matchOn(other)(air => nativeUnit.isInWeaponRange(other.nativeUnit),
        ground => nativeUnit.isInWeaponRange(other.nativeUnit),
        building => nativeUnit.isInWeaponRange(other.nativeUnit))
    else
      super.isInWeaponRange(other)
  }

  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.upgrades.register_!(_ => damage.invalidate())
  }

}

trait ArmedMobile extends Mobile with Weapon {

}

trait Weapon extends Controllable with ArmedUnit {
  self: WrapsUnit =>

  def assumeShotDelayOn(target: MaybeCanDie): Int = !!!("This should never be called")

  override def canDoDamage = true

  def isReadyToFireWeapon = cooldownTimer == 0

  def isAttacking = isStartingToAttack || cooldownTimer > 0

  def cooldownTimer = nativeUnit.getAirWeaponCooldown max nativeUnit.getGroundWeaponCooldown

  def isStartingToAttack = nativeUnit.isStartingAttack
  // air & groundweapon need to override this
  def canAttack(other: MaybeCanDie) = false
  def calculateDamageOn(other: MaybeCanDie, assumeHP: Int, assumeShields: Int,
                        shotCount: Int): DamageSingleAttack = calculateDamageOn(
    other.armor, assumeHP, assumeShields, shotCount)
  def calculateDamageOn(other: Armor, assumeHP: Int, assumeShields: Int,
                        shotCount: Int): DamageSingleAttack = !!!(
    "Forgot to override this")

  def matchOn[X](other: MaybeCanDie)
                (ifAir: AirUnit => X, ifGround: GroundUnit => X, ifBuilding: Building => X) =
    other match {
      case a: AirUnit => ifAir(a)
      case g: GroundUnit => ifGround(g)
      case b: Building => ifBuilding(b)
      case x => !!!(s"Check this $x")
    }
  // needs to be overridden
  def isInWeaponRange(target: MaybeCanDie): Boolean = false

  def weaponRangeRadiusTiles = weaponRangeRadius / 32

  // needs to be overridden
  def weaponRangeRadius: Int = 0

  protected def evalDamage(weapon: WeaponType, damageType: DamageType, hitCount: Int,
                           targetsAir: Boolean) = {
    val level = universe.upgrades.weaponLevelOf(self)
    Damage(weapon.damageAmount(), weapon.damageBonus(), weapon.damageCooldown(), damageType,
      hitCount, level,
      targetsAir)
  }

}

trait MobileRangeWeapon extends RangeWeapon with Mobile {

}

trait RangeWeapon extends Weapon {

}

trait MeleeWeapon extends Weapon {

}

trait GroundAndAirWeapon extends RangeWeapon with GroundWeapon with AirWeapon {

}

trait AirUnit extends Killable with Mobile {
  override def isGroundUnit = false
}

trait GroundUnit extends Killable with Mobile {
  override def isGroundUnit = true

  private val inFerry         = oncePerTick {
    val nu = nativeUnit
    val ferry = nu.getTransport
    if (ferry == null) None
    else ownUnits.byNative(nu).asInstanceOf[Option[TransporterUnit]]
  }
  private val myTransportSize = LazyVal.from(armorType.transportSize)
  // why lazyval?
  private var inFerryLastTick = false
  def gotUnloaded = inFerryLastTick && onGround
  def onGround = !loaded
  def transportSize = myTransportSize.get
  override def postTick(): Unit = {
    super.postTick()
    inFerryLastTick = loaded
  }
  def loaded = inFerry.get.isDefined
}

trait Floating

trait CanBuildAddons extends Building {
  private val myAddonArea = oncePerTick {
    Area(area.lowerRight.movedBy(1, -1), Size(2, 2))
  }
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
  def hasAddonAttached = attached.isDefined
  override def onTick_!(): Unit = {
    super.onTick_!()
    attached.filter(_.isDead).foreach { dead =>
      notifyDetach_!(dead)
    }
  }
  def notifyDetach_!(addon: Addon): Unit = {
    trace(s"Addon $addon detached from $this")
    assert(attached.isDefined)
    assert(attached.contains(addon))
    attached = None
  }
}

object WorkerUnit {
  val gasMiningOrders = {
    Set(Order.HarvestGas,
      Order.MoveToGas, Order.ReturnGas, Order.WaitForGas,
      Order.Harvest1, Order.Harvest2, Order.Harvest3, Order.Harvest4)
  }

  def currentPriority[T <: WorkerUnit](w: UnitWithJob[T]) = {
    var valueOfExcuses = 0
    if (!w.isIdle) valueOfExcuses += 1
    if (w.unit.isCarryingMinerals) valueOfExcuses += 2
    if (w.unit.isInMiningProcess) valueOfExcuses += 1
    PriorityChain(valueOfExcuses)
  }
}

trait WorkerUnit extends Killable with Mobile with GroundUnit with GroundWeapon with Floating {

  def isGatheringGas = WorkerUnit.gasMiningOrders(currentOrder)
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

trait TransporterUnit extends AirUnit {
  private val myPickingUp = oncePerTick {
    nativeUnit.getOrderTarget != null
  }
  private val myLoaded    = oncePerTick {
    nativeUnit.getLoadedUnits.asScala.flatMap { u =>
      ownUnits.byNative(u).asInstanceOf[Option[GroundUnit]]
    }.toSet
  }

  def nearestDropTile = {
    ferryManager.nearestDropPointTo(currentTile)
  }

  def isPickingUp = myPickingUp.get
  def loaded = myLoaded.get
  def isCarrying(gu: GroundUnit) = myLoaded.get(gu)
  def canDropHere = ferryManager.canDropHere(currentTile)

  def hasUnitsLoaded = myLoaded.get.nonEmpty
}

trait Ignored extends WrapsUnit

trait Resource extends BlockingTiles {
  val blockingAreaForMainBuilding = {
    val ul = area.upperLeft.movedBy(-3, -3)
    val lr = area.lowerRight.movedBy(3, 3)
    Area(ul, lr)
  }

  private val remainingResources = oncePerTick
                                   {if (nativeUnit.exists) nativeUnit.getResources else 0}

  def nonEmpty = remaining > 8

  def remaining = remainingResources.get
}

trait ArmedBuilding extends Building with RangeWeapon

trait ImmobileSupplyProvider extends SupplyProvider with Building

trait MobileSupplyProvider extends SupplyProvider with Mobile

trait GasProvider extends Resource with Building

trait ShieldCharger extends AnyUnit

class MineralPatch(unit: APIUnit) extends AnyUnit(unit) with Resource {
  def isBeingMined = nativeUnit.isBeingGathered

  def hasRemainingMinerals = remainingMinerals > 8

  myTilePosition.lockValueForever()

  def remainingMinerals = remaining
}

class VespeneGeysir(unit: APIUnit) extends AnyUnit(unit) with Geysir with Resource {
  myTilePosition.lockValueForever()
}

trait ConcussiveGroundDamage extends GroundWeapon {
  override val groundDamageType = Concussive
}

trait ExplosiveGroundDamage extends GroundWeapon {
  override val groundDamageType = Explosive
}

trait NormalGroundDamage extends GroundWeapon {
  override val groundDamageType = Normal
}

trait ConcussiveAirDamage extends AirWeapon {
  override val airDamageType = Concussive
}

trait ExplosiveAirDamage extends AirWeapon {
  override val airDamageType = Explosive
}

trait NormalAirDamage extends AirWeapon {
  override val airDamageType = Normal
}

trait UpgradeLimitLifter extends Building

trait SupportUnit extends Mobile {
  private val myNearestAlliesWithWeapons = oncePer(Primes.prime47) {
    ownUnits.allMobilesWithWeapons.toArray.sortBy { other =>
      other.centerTile.distanceSquaredTo(centerTile)
    }.toVector
  }

  def nearestAllies = myNearestAlliesWithWeapons.get
}

trait PsiArea extends AnyUnit

class SupplyDepot(unit: APIUnit)
  extends AnyUnit(unit) with ImmobileSupplyProvider with TerranBuilding

class Pylon(unit: APIUnit)
  extends AnyUnit(unit) with Building with PsiArea with ImmobileSupplyProvider

class Overlord(unit: APIUnit)
  extends AnyUnit(unit) with MobileSupplyProvider with TransporterUnit with CanDetectHidden with
          IsBig

class SCV(unit: APIUnit)
  extends AnyUnit(unit) with WorkerUnit with IsSmall with NormalGroundDamage with
          InstantAttackGround

class Probe(unit: APIUnit)
  extends AnyUnit(unit) with WorkerUnit with IsSmall with NormalGroundDamage with
          InstantAttackGround

class Drone(unit: APIUnit)
  extends AnyUnit(unit) with WorkerUnit with IsSmall with Organic with NormalGroundDamage with
          InstantAttackGround

class Shuttle(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit with SupportUnit with IsBig

class Dropship(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit with SupportUnit with IsBig

class CommandCenter(unit: APIUnit)
  extends AnyUnit(unit) with MainBuilding with CanBuildAddons with TerranBuilding

class Nexus(unit: APIUnit) extends AnyUnit(unit) with MainBuilding

class Hive(unit: APIUnit) extends AnyUnit(unit) with MainBuilding

class Assimilator(unit: APIUnit) extends AnyUnit(unit) with Building with GasProvider

class Gateway(unit: APIUnit) extends AnyUnit(unit) with UnitFactory

class PhotonCannon(unit: APIUnit)
  extends AnyUnit(unit) with Building with GroundAndAirWeapon with NormalAirDamage with
          NormalGroundDamage with ArmedBuilding with DetectorBuilding {
  override def damageDelayFactorAir = 1

  override def damageDelayFactorGround = 1
}

class ShieldBattery(unit: APIUnit) extends AnyUnit(unit) with Building with ShieldCharger

class CyberneticsCore(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader

class RoboticsSupportBay(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader

class Observatory(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader

class TemplarArchive(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader

class FleetBeacon(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader

class RoboticsFacility(unit: APIUnit) extends AnyUnit(unit) with UnitFactory with Upgrader

class ArbiterTribunal(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader

class Forge(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader

class Citadel(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader

class Stargate(unit: APIUnit) extends AnyUnit(unit) with UnitFactory

class Comsat(unit: APIUnit)
  extends AnyUnit(unit) with AreaSpellcasterBuilding with Addon with TerranBuilding {
  override type Caster = Comsat
  override val spells: List[SinglePointMagicSpell] = List(ScannerSweep)
}

class NuclearSilo(unit: APIUnit)
  extends AnyUnit(unit) with AreaSpellcasterBuilding with Addon with TerranBuilding {
  override type Caster = NuclearSilo
  override val spells: List[SinglePointMagicSpell] = List(Nuke)
}

class PhysicsLab(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon with TerranBuilding

class Refinery(unit: APIUnit) extends AnyUnit(unit) with GasProvider with TerranBuilding

class CovertOps(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon with TerranBuilding

class MachineShop(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon with TerranBuilding

class ControlTower(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon with TerranBuilding

class Barracks(unit: APIUnit) extends AnyUnit(unit) with UnitFactory with TerranBuilding

class Factory(unit: APIUnit)
  extends AnyUnit(unit) with UnitFactory with CanBuildAddons with TerranBuilding

class Starport(unit: APIUnit)
  extends AnyUnit(unit) with UnitFactory with CanBuildAddons with TerranBuilding

class Academy(unit: APIUnit) extends AnyUnit(unit) with Upgrader with TerranBuilding

class Armory(unit: APIUnit) extends AnyUnit(unit) with Upgrader with TerranBuilding

class EngineeringBay(unit: APIUnit) extends AnyUnit(unit) with Upgrader with TerranBuilding

class ScienceFacility(unit: APIUnit)
  extends AnyUnit(unit) with Upgrader with CanBuildAddons with UpgradeLimitLifter with
          TerranBuilding

class MissileTurret(unit: APIUnit)
  extends AnyUnit(unit) with ArmedBuilding with DetectorBuilding with SlowAttackAir with AirWeapon
          with ExplosiveAirDamage with TerranBuilding

class Bunker(unit: APIUnit) extends AnyUnit(unit) with Building with TerranBuilding

class SporeColony(unit: APIUnit)
  extends AnyUnit(unit) with Building with GroundAndAirWeapon with NormalAirDamage with
          NormalGroundDamage with ArmedBuilding with DetectorBuilding {
  override def damageDelayFactorAir = 1

  override def damageDelayFactorGround = 1
}

trait InstantAttackAir extends AirWeapon {
  override def damageDelayFactorAir = 0
}

trait MediumAttackAir extends AirWeapon {
  override def damageDelayFactorAir = 1
}

trait SlowAttackAir extends AirWeapon {
  override def damageDelayFactorAir = 1
}

trait InstantAttackGround extends GroundWeapon {
  override def damageDelayFactorGround = 0
}

trait MediumAttackGround extends GroundWeapon {
  override def damageDelayFactorGround = 1
}

trait SlowAttackGround extends GroundWeapon {
  override def damageDelayFactorGround = 2
}

trait MobileDetector extends CanDetectHidden with Mobile

trait CanDetectHidden extends WrapsUnit with Detector {
  private val sight = math.round(nativeType.get.sightRange() / 32.0).toInt
  override def detectionRadius = sight
}

trait CanSiege extends Mobile {
  private val sieged = oncePerTick {
    nativeUnit.isSieged
  }
  def isSieged = sieged.get
}

trait CanUseStimpack extends Mobile with Weapon with HasSingleTargetSpells {
  private val stimmed = oncePerTick {nativeUnit.isStimmed || stimTime > 0}
  def isStimmed = stimmed.get
  private def stimTime = nativeUnit.getStimTimer
}

trait PermaCloak extends CanCloak {
  override def isCloaked = true
}

trait CanCloak extends Mobile {
  private val cloaked   = oncePerTick {
    nativeUnit.isCloaked
  }
  private val decloaked = oncePerTick {
    nativeUnit.isDetected
  }
  def isCloaked = cloaked.get

  def isDecloaked = decloaked.get
}

trait DetectorsFirst extends IsTech {
  override def priorityRule: Option[(Mobile) => Double] = Some { m =>
    m.isInstanceOf[Detector].ifElse(1, 0)
  }
}

trait ByPrice extends IsTech {
  override def priorityRule: Option[(Mobile) => Double] = Some(m => m.price.sum)
}

trait CastOn

case object OwnUnits extends CastOn

case object EnemyUnits extends CastOn

abstract class SingleTargetSpell[C <: HasSingleTargetSpells, M <: Mobile : Manifest]
(val tech: Upgrade with SingleTargetMagicSpell) {
  val castRange       = 300
  val castRangeSquare = castRange * castRange

  private val targetClass = tech.canCastOn

  assert(targetClass.isAssignableFrom(manifest[M].runtimeClass),
    s"$targetClass vs ${manifest[M].runtimeClass}")

  def castOn: CastOn = EnemyUnits

  def shouldActivateOn(validated: M) = true

  def casted(m: Mobile) = {
    assert(canBeCastOn(m))
    m.asInstanceOf[M]
  }

  def canBeCastOn(m: Mobile) = targetClass.isInstance(m)

  def isAffected(m: M): Boolean

  def priorityRule = tech.priorityRule
}

object Spells {

  case object Lockdown extends SingleTargetSpell[Ghost, Mechanic](Upgrades.Terran.GhostStop) {
    override def isAffected(m: Mechanic) = {
      m.isLocked
    }
  }

  case object WraithCloak extends SingleTargetSpell[Wraith, Wraith](Upgrades.Terran.WraithCloak) {
    override def isAffected(m: Wraith) = {
      m.isCloaked
    }
  }

  case object TankSiege extends SingleTargetSpell[Tank, Tank](Upgrades.Terran.TankSiegeMode) {
    override def isAffected(m: Tank) = {
      m.isSieged
    }
  }

  case object Stimpack
    extends SingleTargetSpell[CanUseStimpack, CanUseStimpack](Upgrades.Terran.InfantryCooldown) {
    override def isAffected(m: CanUseStimpack) = {
      m.isStimmed
    }
  }

  case object Irradiate
    extends SingleTargetSpell[ScienceVessel, Organic](Upgrades.Terran.Irradiate) {
    override def isAffected(m: Organic) = {
      m.isIrradiated
    }
  }

  case object DefenseMatrix
    extends SingleTargetSpell[ScienceVessel, Mobile](Upgrades.Terran.Defensematrix) {
    override def isAffected(m: Mobile) = m.matrixHp >= 25 // allow refreshing the matrix

    override def castOn = OwnUnits

    override def shouldActivateOn(validated: Mobile) = {
      validated.isBeingAttacked
    }
  }

  case object Blind extends SingleTargetSpell[Medic, Organic](Upgrades.Terran.MedicFlare) {
    override def isAffected(m: Organic) = m.isBlinded
  }

}

trait HasMana extends WrapsUnit {
  def mana = nativeUnit.getEnergy
}

trait HasSingleTargetSpells extends Mobile with HasMana {
  type CasterType <: HasSingleTargetSpells
  val spells: Seq[SingleTargetSpell[CasterType, _]]
  protected val cooldown = 24
  private   var lastCast = -9999
  override def hasSpells = true
  def toOrder(tech: SingleTargetMagicSpell, target: Mobile) = {
    assert(canCastNow(tech))
    lastCast = universe.currentTick
    Orders.TechOnTarget(this, target, tech)
  }
  def canCastNow(tech: SingleTargetMagicSpell) = {
    assert(spells.exists(_.tech == tech))
    def hasMana = tech.energyNeeded <= mana
    def isReadyForCastCool = lastCast + cooldown < universe.currentTick
    hasMana && isReadyForCastCool
  }
}

trait HasSinglePointMagicSpell extends WrapsUnit {

  type Caster <: HasSinglePointMagicSpell
  val spells: List[SinglePointMagicSpell]
  protected val cooldown = 24
  private   var lastCast = -9999
  override def hasSpells = true
  def toOrder(tech: SinglePointMagicSpell, target: MapTilePosition) = {
    assert(canCastNow(tech))
    lastCast = universe.currentTick
    Orders.TechOnTile(this, target, tech)
  }

  def canCastNow(tech: SinglePointMagicSpell) = {
    assert(spells.contains(tech))
    def isReadyForCastCool = lastCast + cooldown < universe.currentTick
    isReadyForCastCool
  }

}

trait Mechanic extends Mobile {

  private val myLocked = oncePerTick {
    nativeUnit.getLockdownTimer > 0
  }

  def isLocked = myLocked.get
}

trait Organic extends Mobile {
  def isBlinded = nativeUnit.isBlind
}

trait IsSmall extends Mobile with MaybeCanDie {
  override val armorType = Small
}

trait IsMedium extends Mobile with MaybeCanDie {
  override val armorType = Medium
}

trait IsBig extends Mobile with MaybeCanDie {
  override val armorType = Large
}

class Zergling(unit: APIUnit)
  extends AnyUnit(unit) with Organic with GroundUnit with GroundWeapon with NormalGroundDamage with
          IsSmall with ArmedMobile with MeleeWeapon with InstantAttackGround

class Broodling(unit: APIUnit)
  extends AnyUnit(unit) with Organic with GroundUnit with GroundWeapon with NormalGroundDamage with
          IsSmall with ArmedMobile with InstantAttackGround

class Hydralisk(unit: APIUnit)
  extends AnyUnit(unit) with Organic with GroundUnit with GroundAndAirWeapon with
          ExplosiveAirDamage with ArmedMobile with
          ExplosiveGroundDamage with IsMedium with InstantAttackAir with InstantAttackGround

class Lurker(unit: APIUnit)
  extends AnyUnit(unit) with Organic with GroundUnit with GroundWeapon with NormalGroundDamage with
          IsBig with ArmedMobile with InstantAttackGround

class Mutalisk(unit: APIUnit)
  extends AnyUnit(unit) with Organic with AirUnit with GroundAndAirWeapon with
          NormalGroundDamage with ArmedMobile with
          NormalAirDamage with IsMedium with InstantAttackAir with InstantAttackGround

class Queen(unit: APIUnit) extends AnyUnit(unit) with Organic with AirUnit with IsMedium

class Scourge(unit: APIUnit)
  extends AnyUnit(unit) with Organic with AirUnit with IsSmall with ArmedMobile

class Guardian(unit: APIUnit)
  extends AnyUnit(unit) with Organic with AirUnit with GroundWeapon with IsBig with
          NormalGroundDamage with ArmedMobile with MediumAttackGround

class Devourer(unit: APIUnit) extends AnyUnit(unit) with Organic with GroundUnit with IsBig

class Ultralisk(unit: APIUnit)
  extends AnyUnit(unit) with Organic with IsBig with GroundWeapon with MeleeWeapon with
          GroundUnit with
          NormalGroundDamage with ArmedMobile with InstantAttackGround

class Defiler(unit: APIUnit) extends AnyUnit(unit) with Organic with GroundUnit with IsMedium

class InfestedTerran(unit: APIUnit)
  extends AnyUnit(unit) with Organic with GroundUnit with ArmedMobile with IsSmall

class Observer(unit: APIUnit)
  extends AnyUnit(unit) with MobileDetector with Mechanic with IsSmall with AirUnit

class Scout(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsBig with
          ExplosiveAirDamage with ArmedMobile with
          NormalGroundDamage with MediumAttackAir with InstantAttackGround

class Zealot(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with IsSmall with IsInfantry with
          NormalGroundDamage with ArmedMobile with InstantAttackGround

class Dragoon(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with Mechanic with IsBig with
          IsVehicle with ArmedMobile with ExplosiveGroundDamage with ExplosiveAirDamage
          with MediumAttackAir with MediumAttackGround

class Archon(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with IsBig with IsInfantry with
          NormalAirDamage with ArmedMobile with NormalGroundDamage with InstantAttackAir with
          InstantAttackGround

class Carrier(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with Mechanic with IsBig with ArmedMobile with IsShip

class Arbiter(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsBig with
          IsShip with ArmedMobile with ExplosiveAirDamage with ExplosiveGroundDamage with
          MediumAttackAir with MediumAttackGround

class Templar(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with HasSingleTargetSpells with IsSmall with
          IsInfantry {
  override val spells = Nil

  override def shouldReRegisterOnMorph = true
}

class DarkTemplar(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with CanCloak with IsSmall with
          IsInfantry with ArmedMobile with
          NormalGroundDamage with InstantAttackGround {
  override def shouldReRegisterOnMorph = true
}

class DarkArchon(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with IsBig with IsInfantry

class Corsair(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with IsMedium with IsShip with
          ExplosiveAirDamage with ArmedMobile with InstantAttackAir

class Interceptor(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsSmall with
          IsShip with ArmedMobile with NormalAirDamage with NormalGroundDamage with
          InstantAttackAir with InstantAttackGround

class Reaver(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with Mechanic with IsBig with
          IsVehicle with ArmedMobile with NormalGroundDamage with SlowAttackGround

class Scarab(unit: APIUnit)
  extends AnyUnit(unit) with SimplePosition with Mobile with AutoPilot with IsSmall with
          GroundUnit with
          IndestructibleUnit

trait BadDancer extends Mobile

class SpiderMine(unit: APIUnit)
  extends AnyUnit(unit) with SimplePosition with GroundUnit with IsSmall with AutoPilot {
  override def canMove = false
}

class Marine(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanUseStimpack with
          MobileRangeWeapon with ArmedMobile with
          IsSmall with IsInfantry with NormalAirDamage with NormalGroundDamage with
          HasSingleTargetSpells with
          InstantAttackAir with InstantAttackGround {
  override type CasterType = CanUseStimpack
  override val spells = List(Spells.Stimpack)
}

class Firebat(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with CanUseStimpack with IsSmall with
          IsInfantry with ArmedMobile with
          HasSingleTargetSpells with InstantAttackGround with ConcussiveGroundDamage {
  override type CasterType = CanUseStimpack
  override val spells = List(Spells.Stimpack)
}

class Ghost(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanCloak with
          InstantAttackAir with ArmedMobile with InstantAttackGround with
          HasSingleTargetSpells with MobileRangeWeapon with IsSmall with IsInfantry with
          ConcussiveAirDamage with ConcussiveGroundDamage {
  override type CasterType = Ghost
  override val spells = List(Spells.Lockdown)
}

class Medic(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with SupportUnit with HasSingleTargetSpells with
          IsSmall with IsInfantry {
  override type CasterType = Medic
  override val spells = List(Spells.Blind)
}

class Vulture(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with HasSpiderMines with
          MediumAttackGround with Mechanic with
          MobileRangeWeapon with IsMedium with IsVehicle with ConcussiveGroundDamage with
          HasSinglePointMagicSpell with ArmedMobile {
  override type Caster = Vulture
  override val spells = List(Upgrades.Terran.SpiderMines)
}

class Tank(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with InstantAttackGround with
          Mechanic with CanSiege with ArmedMobile with
          MobileRangeWeapon with IsBig with IsVehicle with ExplosiveGroundDamage with
          HasSingleTargetSpells {
  override type CasterType = Tank
  override val spells = List(Spells.TankSiege)
}

class Goliath(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with InstantAttackGround with
          SlowAttackAir with Mechanic with MobileRangeWeapon with IsBig with IsVehicle with
          NormalGroundDamage with BadDancer with
          ExplosiveAirDamage with ArmedMobile

class Wraith(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with CanCloak with
          InstantAttackGround with MediumAttackAir with Mechanic with MobileRangeWeapon with
          IsBig with IsShip with NormalGroundDamage with
          ExplosiveAirDamage with ArmedMobile with HasSingleTargetSpells {

  override type CasterType = Wraith
  override val spells = List(Spells.WraithCloak)
}

class Valkery(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with MobileRangeWeapon with
          IsBig with IsShip with ArmedMobile with
          ExplosiveAirDamage with SlowAttackAir

class Battlecruiser(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with InstantAttackAir with
          InstantAttackGround with Mechanic with ArmedMobile with
          HasSingleTargetSpells with MobileRangeWeapon with IsBig with IsShip with
          NormalAirDamage with
          NormalGroundDamage {
  override val spells = Nil
}

class ScienceVessel(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with SupportUnit with CanDetectHidden with Mechanic with
          HasSingleTargetSpells with
          IsBig with IsShip {
  override type CasterType = ScienceVessel
  override val spells = List(Spells.DefenseMatrix, Spells.Irradiate)
}

trait SimplePosition extends WrapsUnit {
  override def center = {
    val p = nativeUnit.getPosition
    MapPosition(p.getX, p.getY)
  }
}

class Irrelevant(unit: APIUnit) extends AnyUnit(unit) {
  override def center = !!!(s"Why did this get called?")

  override def shouldReRegisterOnMorph: Boolean = false
}

trait Geysir extends Resource with BlockingTiles

object UnitWrapper {

  private val mappingRules: Map[UnitType, (APIUnit => WrapsUnit, Class[_ <: WrapsUnit])] =
    HashMap(
      UnitType.Resource_Vespene_Geyser -> ((new VespeneGeysir(_), classOf[VespeneGeysir])),
      UnitType.Terran_Supply_Depot -> ((new SupplyDepot(_), classOf[SupplyDepot])),
      UnitType.Terran_Command_Center -> ((new CommandCenter(_), classOf[CommandCenter])),
      UnitType.Terran_Barracks -> ((new Barracks(_), classOf[Barracks])),
      UnitType.Terran_Academy -> ((new Academy(_), classOf[Academy])),
      UnitType.Terran_Armory -> ((new Armory(_), classOf[Armory])),
      UnitType.Terran_Science_Facility -> ((new ScienceFacility(_), classOf[ScienceFacility])),
      UnitType.Terran_Bunker -> ((new Bunker(_), classOf[Bunker])),
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

      UnitType.Terran_SCV -> ((new SCV(_), classOf[SCV])),
      UnitType.Terran_Firebat -> ((new Firebat(_), classOf[Firebat])),
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
      UnitType.Protoss_Arbiter -> ((new Arbiter(_), classOf[Arbiter])),

      UnitType.Protoss_Nexus -> ((new Nexus(_), classOf[Nexus])),
      UnitType.Protoss_Arbiter_Tribunal -> ((new ArbiterTribunal(_), classOf[ArbiterTribunal])),
      UnitType.Protoss_Assimilator -> ((new Assimilator(_), classOf[Assimilator])),
      UnitType.Protoss_Gateway -> ((new Gateway(_), classOf[Gateway])),
      UnitType.Protoss_Pylon -> ((new Pylon(_), classOf[Pylon])),
      UnitType.Protoss_Citadel_of_Adun -> ((new Citadel(_), classOf[Citadel])),
      UnitType.Protoss_Templar_Archives -> ((new TemplarArchive(_), classOf[TemplarArchive])),
      UnitType.Protoss_Shield_Battery -> ((new ShieldBattery(_), classOf[ShieldBattery])),
      UnitType.Protoss_Cybernetics_Core -> ((new CyberneticsCore(_), classOf[CyberneticsCore])),
      UnitType.Protoss_Fleet_Beacon -> ((new FleetBeacon(_), classOf[FleetBeacon])),
      UnitType.Protoss_Forge -> ((new Forge(_), classOf[Forge])),
      UnitType.Protoss_Photon_Cannon -> ((new PhotonCannon(_), classOf[PhotonCannon])),
      UnitType.Protoss_Robotics_Facility -> ((new RoboticsFacility(_), classOf[RoboticsFacility])),
      UnitType.Protoss_Robotics_Support_Bay ->
      ((new RoboticsSupportBay(_), classOf[RoboticsSupportBay])),
      UnitType.Protoss_Stargate -> ((new Stargate(_), classOf[Stargate])),

      UnitType.Zerg_Drone -> ((new Drone(_), classOf[Drone])),
      UnitType.Zerg_Broodling -> ((new Broodling(_), classOf[Broodling])),
      UnitType.Zerg_Zergling -> ((new Zergling(_), classOf[Zergling])),
      UnitType.Zerg_Defiler -> ((new Defiler(_), classOf[Defiler])),
      UnitType.Zerg_Guardian -> ((new Guardian(_), classOf[Guardian])),
      UnitType.Zerg_Hydralisk -> ((new Hydralisk(_), classOf[Hydralisk])),
      UnitType.Zerg_Mutalisk -> ((new Mutalisk(_), classOf[Mutalisk])),
      UnitType.Zerg_Infested_Terran -> ((new InfestedTerran(_), classOf[InfestedTerran])),
      UnitType.Zerg_Lurker -> ((new Lurker(_), classOf[Lurker])),
      UnitType.Zerg_Queen -> ((new Queen(_), classOf[Queen])),
      UnitType.Zerg_Scourge -> ((new Scourge(_), classOf[Scourge])),
      UnitType.Zerg_Ultralisk -> ((new Ultralisk(_), classOf[Ultralisk])),

      UnitType.Unknown -> ((new Irrelevant(_), classOf[Irrelevant]))
    )

  def class2UnitType = {
    val stupid = (classOf[Tank], UnitType.Terran_Siege_Tank_Tank_Mode)
    mappingRules.map { case (k, (_, c)) =>
      c -> k
    } + stupid
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
