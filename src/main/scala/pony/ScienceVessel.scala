package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class ScienceVessel(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with SupportUnit with CanDetectHidden with Mechanic with HasSingleTargetSpells with IsBig with IsShip {
  override type CasterType = ScienceVessel
  override val spells = List(Spells.DefenseMatrix, Spells.Irradiate)
}
