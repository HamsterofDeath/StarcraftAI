package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait CanUseStimpack extends Mobile with Weapon with HasSingleTargetSpells {
  private val stimmed  = oncePerTick { nativeUnit.isStimmed || stimTime > 0 }
  def isStimmed        = stimmed.get
  private def stimTime = nativeUnit.getStimTimer
}
