package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait Mobile extends WrapsUnit with Controllable {

  override def center = currentTile.asMapPosition

  def isGroundUnit: Boolean
  def asGroundUnit = if (isGroundUnit) this.asInstanceOf[GroundUnit].toSome else None
  def asWithGroundWeapon = if (isGroundUnit) this.asInstanceOf[GroundWeapon].toSome else None
  def canSee(tile: MapTilePosition) = mapLayers.rawWalkableMap.connectedByLine(tile, currentTile)
  val buildPrice = Price(nativeUnit.getType.mineralPrice(), nativeUnit.getType.gasPrice())
  private val myCurrentArea = oncePer(Primes.prime11) {
    mapLayers.rawWalkableMap.areaOf(currentTile).orElse {
      mapLayers.rawWalkableMap
      .spiralAround(currentTile, 2)
      .map(mapLayers.rawWalkableMap.areaOf)
      .find(_.isDefined)
      .map(_.get)
    }
  }
  def isInstantFireUnit = this match {
    case ga: GroundAndAirWeapon => ga.isInstantAttackGround && ga.isInstantAttackAir
    case g: GroundWeapon => g.isInstantAttackGround
    case a: AirWeapon => a.isInstantAttackAir
    case _ => false
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
  private val myArea                = oncePerTick {
    Area(currentTile, armorType.tileSize)
  }
  private var lastFrameMatrixPoints = 0
  def canMove = true
  def currentArea = myCurrentArea.get
  def isAutoPilot = false
  override def isBeingAttacked: Boolean = super.isBeingAttacked || matrixHp < lastFrameMatrixPoints
  def matrixHp = defenseMatrixHP.get
  def hasDefenseMatrix = defenseMatrix.get
  def isIrradiated = irradiation.get

  def isGuarding = currentNativeOrder == Order.PlayerGuard

  def isMoving = nativeUnit.isMoving

  def blockedArea = myArea.get
  def unitTileSize = armor.armorType.tileSize

  override def toString = s"${super.toString}@$currentTile"

  override def onTick_!(): Unit = {
    super.onTick_!()
  }
  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.register_!(() => {
      lastFrameMatrixPoints = nativeUnit.getDefenseMatrixPoints
    })
  }
}
