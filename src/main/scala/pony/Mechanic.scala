package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait Mechanic extends Mobile {

  private val myLocked = oncePerTick {
    nativeUnit.getLockdownTimer > 0
  }

  def isLocked = myLocked.get
}
