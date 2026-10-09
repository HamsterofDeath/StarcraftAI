package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Medic(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with SupportUnit with HasSingleTargetSpells with IsSmall with IsInfantry {
  override type CasterType = Medic
  override val spells = List(Spells.Blind)
}
