package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

trait WrapsUnit extends HasUniverse with AfterTickListener {

  lazy val isEnemy = universe.enemyUnits.byId(nativeUnitId).isDefined &&
    universe.ownUnits.byId(nativeUnitId).isEmpty

  def currentTileNative = currentTile.asNative

  def currentTile = myTile.get

  def isNonFighter = false

  def isFigher = !isNonFighter

  def currentPositionNative = currentPosition.toNative
  def currentPosition       = myCurrentPosition.get

  private val myTile = oncePerTick {
    val tp = nativeUnit.getPosition
    MapTilePosition.shared(tp.getX / 32, tp.getY / 32)
  }
  private val myCurrentPosition = oncePerTick {
    val p = nativeUnit.getPosition
    MapPosition(p.getX, p.getY)
  }

  private val unitId               = WrapsUnit.nextId
  private var morphed              = false
  private var inGame               = true
  private var creationTick         = -1
  private var myUniverse: Universe = uninitialized

  private val myExists = oncePerTick {
    nativeUnit.exists()
  }
  val nativeUnitId         = nativeUnit.getID
  val initialNativeType    = nativeUnit.getType
  private val myNativeType = oncePerTick {
    val ret = nativeUnit.getType
    morphed |= ret != initialNativeType
    ret
  }

  def nativeUnitType = myNativeType.get

  private val myTarget = oncePerTick {
    nativeUnit.getTarget.wrapNull
      .orElse(nativeUnit.getOrderTarget.wrapNull)
      .map(_.getID)
      .flatMap(universe.allUnits.byNativeId)
  }
  private val nativeOrder = oncePerTick {
    nativeUnit.getOrder
  }

  def currentTarget = myTarget.get

  private val curOrder   = oncePerTick { nativeUnit.getOrder }
  private val unfinished = oncePerTick(
    nativeUnit.getRemainingBuildTime > 0 || !nativeUnit.isCompleted
  )
  private val myCenterTile = oncePerTick {
    val c = center
    MapTilePosition(c.x / 32, c.y / 32)
  }

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

  override def postTick(): Unit = {}

  def onMorph(getType: UnitType) = {}

  def shouldReRegisterOnMorph = false

  def currentOrder = curOrder.get

  def age = universe.currentTick - creationTick

  def isInGame = {
    val ct = centerTile
    inGame && ct.isInsideOfGame
  }

  def exists = myExists.get

  def centerTile = myCenterTile.get

  def canDoDamage = false

  def init_!(universe: Universe): Unit = {
    myUniverse = universe
    onUniverseSet(universe)
    creationTick = universe.currentTick
    universe.register_!(this)
  }

  protected def onUniverseSet(universe: Universe): Unit = {}

  def center: MapPosition

  def isSelected = nativeUnit.isSelected
  def nativeUnit: APIUnit
  def shortDebugString = s"$unitIdText"
  def unitIdText       = Integer.toString(unitId, 36)
  def mySCRace         = {
    val r = nativeUnit.getType.getRace
    SCRace.fromNative(r)
  }
  def isBeingCreated      = unfinished.get
  override def onTick_!() = {
    super.onTick_!()
  }

  object surroundings {
    private val near   = 8
    private val medium = 12

    private val myNearEnemyGroundUnits = oncePer(Primes.prime23) {
      universe.unitGrid.enemy.allInRange[GroundUnit](centerTile, near).toVector
    }

    private val myNearEnemyAirUnits = oncePer(Primes.prime23) {
      universe.unitGrid.enemy.allInRange[AirUnit](centerTile, near).toVector
    }

    private val myMediumEnemyGroundUnits = oncePer(Primes.prime23) {
      universe.unitGrid.enemy.allInRange[GroundUnit](centerTile, medium).toVector
    }

    private val myCloseOwnBuildings = oncePer(Primes.prime29) {
      ownUnits.allBuildings.filter(_.centerTile.distanceToIsLess(centerTile, near))
    }

    private val myCloseOwnUnits = oncePer(Primes.prime29) {
      ownUnits.allMobilesWithWeapons.filter(_.centerTile.distanceToIsLess(centerTile, near))
    }

    private val myCloseEnemyBuildings = oncePer(Primes.prime29) {
      enemies.allBuildings.filter(_.centerTile.distanceToIsLess(centerTile, near))
    }

    private val myMediumEnemyBuildings = oncePer(Primes.prime29) {
      enemies.allBuildings.filter(_.centerTile.distanceToIsLess(centerTile, medium))
    }

    def closeEnemyBuildings = myCloseEnemyBuildings.get

    def mediumEnemyBuildings = myMediumEnemyBuildings.get

    def closeEnemyGroundUnits = myNearEnemyGroundUnits.get

    def closeEnemyUnits = myNearEnemyGroundUnits.iterator ++ myNearEnemyAirUnits.iterator

    def mediumEnemyGroundUnits = myMediumEnemyGroundUnits.get

    def closeOwnBuildings = myCloseOwnBuildings.get

    def closeOwnUnits = myCloseOwnUnits.get

  }

}

object WrapsUnit {
  private var counter = 0

  def nextId = {
    val ret = counter
    counter += 1
    ret
  }
}
