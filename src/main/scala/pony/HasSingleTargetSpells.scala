package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait HasSingleTargetSpells extends Mobile with HasMana {
  type CasterType <: HasSingleTargetSpells
  val spells: Seq[SingleTargetSpell[CasterType, ?]]
  protected val cooldown                                    = 24
  private var lastCast                                      = -9999
  override def hasSpells                                    = true
  def toOrder(tech: SingleTargetMagicSpell, target: Mobile) = {
    assert(canCastNow(tech))
    lastCast = universe.currentTick
    Orders.TechOnTarget(this, target, tech)
  }
  def canCastNow(tech: SingleTargetMagicSpell) = {
    assert(spells.exists(_.tech == tech))
    def hasMana            = tech.energyNeeded <= mana
    def isReadyForCastCool = lastCast + cooldown < universe.currentTick
    hasMana && isReadyForCastCool
  }
}
