package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

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
