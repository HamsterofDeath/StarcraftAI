package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Comsat(unit: APIUnit)
    extends AnyUnit(unit) with AreaSpellcasterBuilding with Addon with TerranBuilding {
  override type Caster = Comsat
  override val spells: List[SinglePointMagicSpell] = List(ScannerSweep)
}
