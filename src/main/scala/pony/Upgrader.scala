package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait Upgrader extends Controllable with Building {
  def canUpgrade(upgrade: Upgrade) = race.techTree.canUpgrade(getClass, upgrade)
  def isDoingResearch              = currentOrder == Order.ResearchTech || currentOrder == Order.Upgrade
}
