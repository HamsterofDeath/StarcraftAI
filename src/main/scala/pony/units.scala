package pony

import bwapi.{Order, Race, TechType, Unit => APIUnit, UnitType, UpgradeType, WeaponType}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait NiceToString extends WrapsUnit {
  override def toString = s"[$unitIdText] ${getClass.className}"
}

abstract class AnyUnit(val nativeUnit: APIUnit) extends WrapsUnit with NiceToString with OrderHistorySupport {

}

trait OrderHistorySupport extends WrapsUnit {
  private val history    = ListBuffer.empty[HistoryElement]
  private val maxHistory = 1000
  def trackOrder(order: UnitOrder): Unit = {
    history.lastOption.foreach(_.trackOrder_!(order))
  }
  override def onTick(): Unit = {
    super.onTick()
    if (universe.world.debugger.isDebugging) {
      if (universe.unitManager.hasJob(this)) {
        history += HistoryElement(nativeUnit.getOrder, nativeUnit.getOrderTarget, universe.unitManager.jobOf(this))
        if (history.size > maxHistory) {
          history.remove(0)
        }
      }
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

trait WrapsUnit extends HasUniverse with HasLazyVals with AfterTickListener {

  private var inGame = true

  def notifyRemoved_!(): Unit = {
    inGame = false
    universe.unregister_!(this)
  }

  override def postTick(): Unit = {

  }

  def onMorph(getType: UnitType) = {

  }

  def shouldReRegisterOnMorph: Boolean

  def currentOrder = curOrder.get

  def age = universe.currentTick - creationTick
  def isInGame = inGame
  def canDoDamage = false

  private val curOrder = oncePerTick {nativeUnit.getOrder}

  private var creationTick = -1
  val unitId            = WrapsUnit.nextId
  val nativeUnitId      = nativeUnit.getID
  val initialNativeType = nativeUnit.getType
  private var myUniverse: Universe = _

  override def universe: Universe = {
    assert(myUniverse != null)
    myUniverse
  }

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
  private val unfinished = oncePerTick(nativeUnit.getRemainingBuildTime > 0 || !nativeUnit.isCompleted)

  def isBeingCreated = unfinished.get
  override def onTick() = {
    super.onTick()
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
  override def shouldReRegisterOnMorph = true
  def tilePosition = myTilePosition.get
  val myTilePosition = oncePerTick {
    val position = nativeUnit.getTilePosition
    val x = position.getX
    val y = position.getY
    MapTilePosition.shared(x, y)
  }
  val size           = Size.shared(nativeUnit.getType.tileWidth(), nativeUnit.getType.tileHeight())
  private val myArea = oncePerTick {
    Area(tilePosition, size)
  }
  val area = myArea.get
  def nativeMapPosition = tilePosition.asMapPosition.toNative
  override def center = area.center

  override def toString: String = {
    s"${super.toString}@${area.describe}"
  }
}

trait BlockingTiles extends StaticallyPositioned {

}

trait IsInfantry extends WrapsUnit
trait IsVehicle extends WrapsUnit
trait IsShip extends WrapsUnit

trait Building extends BlockingTiles with MaybeCanDie {
  def isFloating = nativeUnit.isFlying

  override val armorType = Building

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

  def energyCost = nativeType.fold(_ => throw new UnsupportedOperationException(s"Called on $this"), _.energyCost())

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

    case object WraithEnergy extends Upgrade(UpgradeType.Apollo_Reactor)
    case object ShipArmor extends Upgrade(UpgradeType.Terran_Ship_Plating)
    case object VehicleArmor extends Upgrade(UpgradeType.Terran_Vehicle_Plating)
    case object InfantryArmor extends Upgrade(UpgradeType.Terran_Infantry_Armor)
    case object InfantryCooldown extends Upgrade(TechType.Stim_Packs) with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanUseStimpack]
    }
    case object InfantryWeapons extends Upgrade(UpgradeType.Terran_Infantry_Weapons)
    case object VehicleWeapons extends Upgrade(UpgradeType.Terran_Vehicle_Weapons)
    case object ShipWeapons extends Upgrade(UpgradeType.Terran_Ship_Weapons)
    case object MarineRange extends Upgrade(UpgradeType.U_238_Shells)
    case object MedicEnergy extends Upgrade(UpgradeType.Caduceus_Reactor)
    case object MedicFlare
      extends Upgrade(TechType.Optical_Flare) with SingleTargetMagicSpell with CastOnOrganic with DetectorsFirst
    case object MedicHeal extends Upgrade(TechType.Restoration) with SingleTargetMagicSpell with CastOnAll
    case object GoliathRange extends Upgrade(UpgradeType.Charon_Boosters)
    case object SpiderMines extends Upgrade(TechType.Spider_Mines) with SinglePointMagicSpell with CastAtFreeTile
    case object Defensematrix extends Upgrade(TechType.Defensive_Matrix) with SingleTargetMagicSpell with CastOnAll
    case object VultureSpeed extends Upgrade(UpgradeType.Ion_Thrusters)
    case object TankSiegeMode extends Upgrade(TechType.Tank_Siege_Mode) with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanSiege]
    }
    case object EMP extends Upgrade(TechType.EMP_Shockwave) with SingleTargetMagicSpell with CastOnAll
    case object Irradiate extends Upgrade(TechType.Irradiate) with SingleTargetMagicSpell with CastOnAll
    case object ScienceVesselEnergy extends Upgrade(UpgradeType.Titan_Reactor)
    case object GhostStop
      extends Upgrade(TechType.Lockdown) with SingleTargetMagicSpell with CastOnMechanic with ByPrice
    case object GhostVisiblityRange extends Upgrade(UpgradeType.Ocular_Implants)
    case object GhostEnergy extends Upgrade(UpgradeType.Moebius_Reactor)
    case object GhostCloak
      extends Upgrade(TechType.Personnel_Cloaking) with PermanentSpell with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanCloak]
    }
    case object WraithCloak
      extends Upgrade(TechType.Cloaking_Field) with PermanentSpell with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanCloak]
    }
    case object CruiserGun extends Upgrade(TechType.Yamato_Gun) with SingleTargetMagicSpell with CastOnAll
    case object CruiserEnergy extends Upgrade(UpgradeType.Colossus_Reactor)

    val allTech = InfantryCooldown :: MedicFlare :: MedicHeal :: SpiderMines :: Defensematrix ::
                  TankSiegeMode :: EMP :: Irradiate :: GhostStop :: GhostCloak :: WraithCloak :: CruiserGun :: Nil

  }

  def allTech = Terran.allTech

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
    else !!!(s"Unknown damage type :(")
  }
}

sealed trait ArmorType {
  def transportSize: Int

  def damageFactorIfHitBy(damageType: DamageType): DamageFactor
}

case object Small extends ArmorType {
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
  override def damageFactorIfHitBy(damageType: DamageType) = Zero
  override def transportSize = !!!("This should never happen")
}
case object Large extends ArmorType {
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

  private val maxHp      = nativeUnit.getType.maxHitPoints()
  private val maxShields = nativeUnit.getType.maxShields()

  def percentageHPOk = {
    hitPoints.sum.toDouble / (maxHp + maxShields)
  }

  def isDamaged = isInGame && (hitPoints.shield < maxShields || hitPoints.hitpoints < maxHp)


  def isHarmlessNow = isIncapacitated || !canDoDamage

  private var lastFrameHp = HitPoints(-1, -1) // obviously wrong, but that doesn't matter

  private def currentHp = myArmor.get.hp

  def isBeingAttacked = currentHp < lastFrameHp

  private val disabled = oncePerTick {evalLocked}

  private def evalLocked = nativeUnit.isLockedDown || nativeUnit.isStasised

  def isIncapacitated = disabled.get

  val armorType: ArmorType

  val price = Price(nativeUnit.getType.mineralPrice(), nativeUnit.getType.gasPrice())

  private var dead = false
  def isDead = dead || currentHp.isDead

  override def isInGame: Boolean = super.isInGame && !isDead

  def notifyDead_!(): Unit = {
    dead = true
  }

  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.register_!(() => {
      lastFrameHp = currentHp
    })
  }

  private val myArmor = oncePerTick {
    val hp = HitPoints(nativeUnit.getHitPoints, nativeUnit.getShields)
    val armorLevel = universe.upgrades.armorForUnitType(self)
    Armor(armorType, hp, armorLevel, self)
  }

  def matchThis[X](ifMobile: Mobile => X, ifBuilding: Building => X) = this match {
    case m: Mobile => ifMobile(m)
    case b: Building => ifBuilding(b)
    case x => !!!(s"Check this $x")
  }

  def hitPoints = currentHp

  def armor = myArmor.get
}

case class Price(minerals: Int, gas: Int) {
  val sum = minerals + gas
}

trait IndestructibleUnit extends WrapsUnit {

}

trait AutoPilot extends Mobile {
  override def isAutoPilot = true
  def isManuallyControlled = !isAutoPilot
}

trait Mobile extends WrapsUnit with Controllable {

  def currentArea = myCurrentArea.get

  private val myCurrentArea = oncePerTick {
    mapLayers.rawWalkableMap.areaWhichContainsAsFree(currentTile).orElse {
      mapLayers.rawWalkableMap
      .spiralAround(currentTile, 2)
      .view
      .map(mapLayers.rawWalkableMap.areaWhichContainsAsFree)
      .find(_.isDefined)
      .map(_.get)
    }
  }

  override def shouldReRegisterOnMorph = false
  def isAutoPilot = false

  private val defenseMatrix = oncePerTick {
    nativeUnit.getDefenseMatrixPoints > 0 || nativeUnit.getDefenseMatrixTimer > 0
  }

  private val defenseMatrixHP = oncePerTick {
    nativeUnit.getDefenseMatrixPoints
  }

  def matrixHp = defenseMatrixHP.get

  override def isBeingAttacked: Boolean = super.isBeingAttacked || matrixHp < lastFrameMatrixPoints

  private var lastFrameMatrixPoints = 0

  private val irradiation = oncePerTick {
    nativeUnit.getIrradiateTimer > 0
  }

  def hasDefenseMatrix = defenseMatrix.get

  def isIrradiated = irradiation.get

  override def center = currentPosition

  def isGuarding = nativeUnit.getOrder == Order.PlayerGuard

  def isMoving = nativeUnit.isMoving

  def currentTileNative = currentTile.asNative

  private val myTile = oncePerTick {
    val tp = nativeUnit.getPosition
    MapTilePosition.shared(tp.getX / 32, tp.getY / 32)
  }

  private val myArea = oncePerTick {
    Area(myTile.get, Size.shared(1, 1))
  }

  def currentTile = myTile.get

  def blockedArea = myArea.get

  def currentPositionNative = currentPosition.toNative

  private val myCurrentPosition = oncePerTick {
    val p = nativeUnit.getPosition
    MapPosition(p.getX, p.getY)
  }

  def currentPosition = myCurrentPosition.get


  override def toString = s"${super.toString}@$currentTile"

  override def onTick(): Unit = {
    super.onTick()
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

trait Detector {

}

trait ArmedUnit extends WrapsUnit {

}

trait HasSpiderMines extends WrapsUnit {
  private val mines = oncePerTick {nativeUnit.getSpiderMineCount}
  def spiderMineCount = mines.get
}

trait GroundWeapon extends Weapon {
  def damageDelayFactorGround: Int

  private val groundWeapon = initialNativeType.groundWeapon()
  val groundRange            = groundWeapon.maxRange()
  val groundCanAttackAir     = groundWeapon.targetsAir()
  val groundCanAttackGround  = groundWeapon.targetsGround()
  val groundDamageMultiplier = groundWeapon.damageFactor()
  val groundDamageType: DamageType

  override def weaponRangeRadius: Int = super.weaponRangeRadius max groundRange

  override def assumeShotDelayOn(target: MaybeCanDie) = {
    if (canAttack(target)) {
      damageDelayFactorGround
    } else
      super.assumeShotDelayOn(target)
  }

  private val damage = LazyVal.from(evalDamage(groundWeapon, groundDamageType, groundDamageMultiplier, false))

  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.upgrades.register_!(_ => damage.invalidate())
  }

  override def canAttack(other: MaybeCanDie) = {
    super.canAttack(other) || selfCanAttack(other)
  }

  private def selfCanAttack(other: MaybeCanDie) = {
    matchOn[Boolean](other)(
    _ => groundCanAttackAir,
    _ => groundCanAttackGround,
    b => if (b.isFloating) groundCanAttackAir else groundCanAttackGround)
  }

  override def calculateDamageOn(other: Armor, assumeHP: Int, assumeShields: Int, shotCount: Int) = {
    if (selfCanAttack(other.owner)) {
      damage.get.damageIfHits(other, assumeHP, assumeShields, shotCount)
    } else {
      super.calculateDamageOn(other, assumeHP, assumeShields, shotCount)
    }
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
      val afterShot = if (hitCount == 2) {
        val hpAfterFirstHit = hp - damageOfFirstHit.onHp
        val shieldAfterFirstHit = shields - damageOfFirstHit.onShields
        val damageOfSecondHit = calculate(other, hpAfterFirstHit, shieldAfterFirstHit)
        DamageSingleAttack(damageOfFirstHit.onHp + damageOfSecondHit.onHp,
          damageOfFirstHit.onShields + damageOfSecondHit.onShields, isAir)
      } else {
        assert(hitCount == 1)
        damageOfFirstHit
      }

      hp -= afterShot.onHp
      shields -= afterShot.onShields
      hpDamage += afterShot.onHp
      shieldDamage += afterShot.onShields

    }

    DamageSingleAttack(hpDamage, shieldDamage, isAir)
  }
}

case class DamageSingleAttack(onHp: Int, onShields: Int, airHit: Boolean)

trait AirWeapon extends Weapon {
  def damageDelayFactorAir: Int

  private val airWeapon = initialNativeType.airWeapon()
  val airRange            = airWeapon.maxRange()
  val airCanAttackAir     = airWeapon.targetsAir()
  val airCanAttackGround  = airWeapon.targetsGround()
  val airDamageMultiplier = airWeapon.damageFactor()
  val airDamageType: DamageType

  override def weaponRangeRadius: Int = super.weaponRangeRadius max airRange

  override def assumeShotDelayOn(target: MaybeCanDie) = {
    if (canAttack(target)) {
      damageDelayFactorAir
    } else
      super.assumeShotDelayOn(target)
  }

  private val damage = LazyVal.from(evalDamage(airWeapon, airDamageType, airDamageMultiplier, true))

  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.upgrades.register_!(_ => damage.invalidate())
  }
  // air & groundweapon need to override this
  override def canAttack(other: MaybeCanDie) = {
    super.canAttack(other) || selfCanAttack(other)
  }

  private def selfCanAttack(other: MaybeCanDie) = {
    matchOn(other)(
      _ => airCanAttackAir,
      _ => airCanAttackGround,
      b => if (b.isFloating) airCanAttackAir else airCanAttackGround)
  }

  override def calculateDamageOn(other: Armor, assumeHP: Int, assumeShields: Int, shotCount: Int) = {
    if (selfCanAttack(other.owner)) {
      damage.get.damageIfHits(other, assumeHP, assumeShields, shotCount)
    } else {
      super.calculateDamageOn(other, assumeHP, assumeShields, shotCount)
    }
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

}

trait Weapon extends Controllable {
  self: WrapsUnit =>

  def assumeShotDelayOn(target: MaybeCanDie): Int = !!!("This should never be called")

  override def canDoDamage = true

  def isReadyToFireWeapon = cooldownTimer == 0
  def cooldownTimer = nativeUnit.getAirWeaponCooldown max nativeUnit.getGroundWeaponCooldown
  def isAttacking = isStartingToAttack || cooldownTimer > 0
  def isStartingToAttack = nativeUnit.isStartingAttack
  // air & groundweapon need to override this
  def canAttack(other: MaybeCanDie) = false
  def calculateDamageOn(other: MaybeCanDie, assumeHP: Int, assumeShields: Int,
                        shotCount: Int): DamageSingleAttack = calculateDamageOn(
    other.armor, assumeHP, assumeShields, shotCount)
  def calculateDamageOn(other: Armor, assumeHP: Int, assumeShields: Int, shotCount: Int): DamageSingleAttack = !!!(
    "Forgot to override this")

  def matchOn[X](other: MaybeCanDie)
                (ifAir: AirUnit => X, ifGround: GroundUnit => X, ifBuilding: Building => X) = other match {
    case a: AirUnit => ifAir(a)
    case g: GroundUnit => ifGround(g)
    case b: Building => ifBuilding(b)
    case x => !!!(s"Check this $x")
  }

  protected def evalDamage(weapon: WeaponType, damageType: DamageType, hitCount: Int, targetsAir: Boolean) = {
    val level = universe.upgrades.weaponLevelOf(self)
    Damage(weapon.damageAmount(), weapon.damageBonus(), weapon.damageCooldown(), damageType, hitCount, level,
      targetsAir)
  }

  def isInWeaponRange(target: MaybeCanDie): Boolean = false // needs to be overriden to return true

  def weaponRangeRadius = 0

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

}

trait GroundUnit extends Killable with Mobile {

  private var inFerryLastTick = false

  def gotUnloaded = inFerryLastTick && onGround

  def onGround = !loaded

  private val inFerry = oncePerTick {
    val nu = nativeUnit
    val ferry = nu.getTransport
    if (ferry == null) None
    else ownUnits.byNative(nu).asInstanceOf[Option[TransporterUnit]]
  }

  def loaded = inFerry.get.isDefined

  lazy val transportSize = armorType.transportSize
  override def postTick(): Unit = {
    super.postTick()
    inFerryLastTick = loaded
  }
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
    assert(attached.contains(addon))
    attached = None
  }
  def hasAddonAttached = attached.isDefined
  override def onTick(): Unit = {
    super.onTick()
    attached.filter(_.isDead).foreach { dead =>
      notifyDetach_!(dead)
    }
  }
}

trait WorkerUnit extends Killable with Mobile with GroundUnit with GroundWeapon with Floating {
  def isGatheringGas = currentOrder == Order.HarvestGas || currentOrder == Order.MoveToGas ||
                       currentOrder == Order.ReturnGas || currentOrder == Order.WaitForGas ||
                       currentOrder == Order.Harvest1 || currentOrder == Order.Harvest2 ||
                       currentOrder == Order.Harvest3 || currentOrder == Order.Harvest4

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
  private val carrying = oncePerTick {
    nativeUnit.getLoadedUnits.asScala.flatMap { u =>
      ownUnits.byNative(u)
    }.toSeq
  }

  def hasUnitsLoaded = carrying.get.nonEmpty
}

trait Ignored extends WrapsUnit

trait Resource extends BlockingTiles {
  val blockingAreaForMainBuilding = {
    val ul = area.upperLeft.movedBy(-3, -3)
    val lr = area.lowerRight.movedBy(3, 3)
    Area(ul, lr)
  }

  private val remainingResources = oncePerTick {if (nativeUnit.exists) nativeUnit.getResources else 0}
  def remaining = remainingResources.get
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

trait SupportUnit extends Mobile

class SupplyDepot(unit: APIUnit) extends AnyUnit(unit) with ImmobileSupplyProvider
class Pylon(unit: APIUnit) extends AnyUnit(unit) with ImmobileSupplyProvider
class Overlord(unit: APIUnit)
  extends AnyUnit(unit) with MobileSupplyProvider with TransporterUnit with CanDetectHidden with IsBig

class SCV(unit: APIUnit)
  extends AnyUnit(unit) with WorkerUnit with IsSmall with NormalGroundDamage with InstantAttackGround
class Probe(unit: APIUnit)
  extends AnyUnit(unit) with WorkerUnit with IsSmall with NormalGroundDamage with InstantAttackGround
class Drone(unit: APIUnit)
  extends AnyUnit(unit) with WorkerUnit with IsSmall with Organic with NormalGroundDamage with InstantAttackGround

class Shuttle(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit with SupportUnit with IsBig
class Dropship(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit with SupportUnit with IsBig

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

class Barracks(unit: APIUnit) extends AnyUnit(unit) with UnitFactory
class Factory(unit: APIUnit) extends AnyUnit(unit) with UnitFactory with CanBuildAddons
class Starport(unit: APIUnit) extends AnyUnit(unit) with UnitFactory with CanBuildAddons

class Academy(unit: APIUnit) extends AnyUnit(unit) with Upgrader
class Armory(unit: APIUnit) extends AnyUnit(unit) with Upgrader
class EngineeringBay(unit: APIUnit) extends AnyUnit(unit) with Upgrader
class ScienceFacility(unit: APIUnit) extends AnyUnit(unit) with Upgrader with CanBuildAddons with UpgradeLimitLifter

class MissileTurret(unit: APIUnit)
  extends AnyUnit(unit) with ArmedBuilding with CanDetectHidden with SlowAttackAir with AirWeapon with
          ExplosiveAirDamage

class Bunker(unit: APIUnit) extends AnyUnit(unit) with Building

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
trait CanDetectHidden extends WrapsUnit
trait CanSiege extends Mobile {
  private val sieged = oncePerTick {
    nativeUnit.isSieged
  }
  def isSieged = sieged.get
}

trait CanUseStimpack extends Mobile with Weapon with HasSingleTargetSpells {
  private val stimmed = LazyVal.from(nativeUnit.isStimmed || stimTime > 0)
  def isStimmed = stimmed.get
  private def stimTime = nativeUnit.getStimTimer
}

trait PermaCloak extends CanCloak {
  override def isCloaked = true
}
trait CanCloak extends Mobile {
  private val cloaked = oncePerTick {
    nativeUnit.isCloaked
  }
  def isCloaked = cloaked.get
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

abstract class SingleTargetSpell[C <: HasSingleTargetSpells, M <: Mobile : Manifest](val tech: Upgrade with
  SingleTargetMagicSpell) {
  private val targetClass = tech.canCastOn

  def castOn: CastOn = EnemyUnits

  assert(targetClass.isAssignableFrom(manifest[M].runtimeClass), s"$targetClass vs ${manifest[M].runtimeClass}")

  val castRange = 300

  val castRangeSquare = castRange * castRange

  def shouldActivateOn(validated: M) = true

  def canBeCastOn(m: Mobile) = targetClass.isAssignableFrom(m.getClass)
  def casted(m: Mobile) = {
    assert(canBeCastOn(m))
    m.asInstanceOf[M]
  }

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

  case object Stimpack extends SingleTargetSpell[CanUseStimpack, CanUseStimpack](Upgrades.Terran.InfantryCooldown) {
    override def isAffected(m: CanUseStimpack) = {
      m.isStimmed
    }
  }

  case object Irradiate extends SingleTargetSpell[ScienceVessel, Organic](Upgrades.Terran.Irradiate) {
    override def isAffected(m: Organic) = {
      m.isIrradiated
    }
  }

  case object DefenseMatrix extends SingleTargetSpell[ScienceVessel, Mobile](Upgrades.Terran.Defensematrix) {
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
  private   var lastCast = -9999
  protected val cooldown = 24

  def canCastNow(tech: SingleTargetMagicSpell) = {
    assert(spells.exists(_.tech == tech))
    def hasMana = tech.energyNeeded <= mana
    def isReadyForCastCool = lastCast + cooldown < universe.currentTick
    hasMana && isReadyForCastCool
  }

  def toOrder(tech: SingleTargetMagicSpell, target: Mobile) = {
    assert(canCastNow(tech))
    lastCast = universe.currentTick
    Orders.TechOnTarget(this, target, tech)
  }
}

trait HasSinglePointMagicSpell extends Mobile {
  type Caster <: HasSinglePointMagicSpell

  private   var lastCast = -9999
  protected val cooldown = 24

  def canCastNow(tech: SinglePointMagicSpell) = {
    assert(spells.contains(tech))
    def isReadyForCastCool = lastCast + cooldown < universe.currentTick
    isReadyForCastCool
  }

  def toOrder(tech: SinglePointMagicSpell, target: MapTilePosition) = {
    assert(canCastNow(tech))
    lastCast = universe.currentTick
    Orders.TechOnTile(this, target, tech)
  }

  val spells: List[SinglePointMagicSpell]

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
  extends AnyUnit(unit) with Organic with GroundUnit with GroundWeapon with NormalGroundDamage with IsSmall with
          MeleeWeapon with InstantAttackGround
class Broodling(unit: APIUnit)
  extends AnyUnit(unit) with Organic with GroundUnit with GroundWeapon with NormalGroundDamage with IsSmall with
          InstantAttackGround
class Hydralisk(unit: APIUnit)
  extends AnyUnit(unit) with Organic with GroundUnit with GroundAndAirWeapon with ExplosiveAirDamage with
          ExplosiveGroundDamage with IsMedium with InstantAttackAir with InstantAttackGround
class Lurker(unit: APIUnit)
  extends AnyUnit(unit) with Organic with GroundUnit with GroundWeapon with NormalGroundDamage with IsBig with
          InstantAttackGround
class Mutalisk(unit: APIUnit)
  extends AnyUnit(unit) with Organic with AirUnit with GroundAndAirWeapon with NormalGroundDamage with
          NormalAirDamage with IsMedium with InstantAttackAir with InstantAttackGround
class Queen(unit: APIUnit) extends AnyUnit(unit) with Organic with AirUnit with IsMedium
class Scourge(unit: APIUnit) extends AnyUnit(unit) with Organic with AirUnit with IsSmall
class Guardian(unit: APIUnit)
  extends AnyUnit(unit) with Organic with AirUnit with GroundWeapon with IsBig with NormalGroundDamage with
          MediumAttackGround
class Devourer(unit: APIUnit) extends AnyUnit(unit) with Organic with GroundUnit with IsBig
class Ultralisk(unit: APIUnit)
  extends AnyUnit(unit) with Organic with IsBig with GroundWeapon with MeleeWeapon with NormalGroundDamage with
          InstantAttackGround
class Defiler(unit: APIUnit) extends AnyUnit(unit) with Organic with GroundUnit with IsMedium
class InfestedTerran(unit: APIUnit) extends AnyUnit(unit) with Organic with GroundUnit with IsSmall

class Observer(unit: APIUnit) extends AnyUnit(unit) with MobileDetector with Mechanic with IsSmall with AirUnit
class Scout(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsBig with ExplosiveAirDamage with
          NormalGroundDamage with MediumAttackAir with InstantAttackGround
class Zealot(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with IsSmall with IsInfantry with NormalGroundDamage with
          InstantAttackGround
class Dragoon(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with Mechanic with IsBig with IsVehicle with
          ExplosiveGroundDamage with ExplosiveAirDamage with MediumAttackAir with MediumAttackGround
class Archon(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with IsBig with IsInfantry with NormalAirDamage with
          NormalGroundDamage with InstantAttackAir with InstantAttackGround
class Carrier(unit: APIUnit) extends AnyUnit(unit) with AirUnit with Mechanic with IsBig with IsShip
class Arbiter(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsBig with IsShip with
          ExplosiveAirDamage with ExplosiveGroundDamage with MediumAttackAir with MediumAttackGround
class Templar(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with PermaCloak with HasSingleTargetSpells with IsSmall with IsInfantry {
  override val spells = Nil
}
class DarkTemplar(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with CanCloak with IsSmall with IsInfantry with
          NormalGroundDamage with InstantAttackGround
class DarkArchon(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with IsBig with IsInfantry
class Corsair(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with IsMedium with IsShip with ExplosiveAirDamage with
          InstantAttackAir
class Interceptor(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsSmall with IsShip with
          NormalAirDamage with NormalGroundDamage with InstantAttackAir with InstantAttackGround
class Reaver(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with Mechanic with IsBig with IsVehicle with
          NormalGroundDamage with SlowAttackGround

class Scarab(unit: APIUnit)
  extends AnyUnit(unit) with SimplePosition with Mobile with AutoPilot with IsSmall with GroundUnit with
          IndestructibleUnit
class SpiderMine(unit: APIUnit)
  extends AnyUnit(unit) with IndestructibleUnit with SimplePosition with GroundUnit with IsSmall with AutoPilot

class Marine(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanUseStimpack with MobileRangeWeapon with
          IsSmall with IsInfantry with NormalAirDamage with NormalGroundDamage with HasSingleTargetSpells with
          InstantAttackAir with InstantAttackGround {
  override type CasterType = CanUseStimpack
  override val spells = List(Spells.Stimpack)
}
class Firebat(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with CanUseStimpack with IsSmall with IsInfantry with
          HasSingleTargetSpells with InstantAttackGround with ConcussiveGroundDamage {
  override type CasterType = CanUseStimpack
  override val spells = List(Spells.Stimpack)
}

class Ghost(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanCloak with InstantAttackAir with
          InstantAttackGround with
          HasSingleTargetSpells with MobileRangeWeapon with IsSmall with IsInfantry with ConcussiveAirDamage with
          ConcussiveGroundDamage {
  override val spells = List(Spells.Lockdown)
  override type CasterType = Ghost
}
class Medic(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with SupportUnit with HasSingleTargetSpells with IsSmall with IsInfantry {
  override val spells = List(Spells.Blind)
  override type CasterType = Medic
}
class Vulture(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with HasSpiderMines with MediumAttackGround with Mechanic with
          MobileRangeWeapon with IsMedium with IsVehicle with ConcussiveGroundDamage with HasSinglePointMagicSpell {
  override val spells = List(Upgrades.Terran.SpiderMines)
  override type Caster = Vulture
}

class Tank(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with InstantAttackGround with Mechanic with CanSiege with
          MobileRangeWeapon with IsBig with IsVehicle with ExplosiveGroundDamage with HasSingleTargetSpells {
  override type CasterType = Tank
  override val spells = List(Spells.TankSiege)
}
class Goliath(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with InstantAttackGround with SlowAttackAir with
          Mechanic with
          MobileRangeWeapon with IsBig with IsVehicle with NormalGroundDamage with ExplosiveAirDamage
class Wraith(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with CanCloak with InstantAttackGround with
          MediumAttackAir with Mechanic with
          MobileRangeWeapon with IsBig with IsShip with NormalGroundDamage with ExplosiveAirDamage with
          HasSingleTargetSpells {

  override type CasterType = Wraith
  override val spells = List(Spells.WraithCloak)
}
class Valkery(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with MobileRangeWeapon with IsBig with IsShip with
          ExplosiveAirDamage with SlowAttackAir
class Battlecruiser(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with InstantAttackAir with InstantAttackGround with
          Mechanic with
          HasSingleTargetSpells with MobileRangeWeapon with IsBig with IsShip with NormalAirDamage with
          NormalGroundDamage {
  override val spells = Nil
}
class ScienceVessel(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with SupportUnit with CanDetectHidden with Mechanic with HasSingleTargetSpells with
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