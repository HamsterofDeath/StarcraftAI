package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait GroundUnit extends Killable with Mobile {
  override def isGroundUnit = true

  private val inFerry = oncePerTick {
    val nu    = nativeUnit
    val ferry = nu.getTransport
    if (ferry == null) None
    else ownUnits.byNative(nu).asInstanceOf[Option[TransporterUnit]]
  }
  private val myTransportSize = LazyVal.from(armorType.transportSize)
  // why lazyval?
  private var inFerryLastTick   = false
  def gotUnloaded               = inFerryLastTick && onGround
  def onGround                  = !loaded
  def transportSize             = myTransportSize.get
  override def postTick(): Unit = {
    super.postTick()
    inFerryLastTick = loaded
  }
  def loaded = inFerry.isDefined
}
