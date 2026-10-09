package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait AreaSpellcasterBuilding
    extends Building with Controllable with HasSinglePointMagicSpell with HasMana {

  override def canCastNow(tech: SinglePointMagicSpell) = {
    def hasMana = tech.energyNeeded <= mana
    super.canCastNow(tech) && hasMana
  }
}
