package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Marine(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanUseStimpack with MobileRangeWeapon
    with ArmedMobile with IsSmall with IsInfantry with NormalAirDamage with NormalGroundDamage
    with HasSingleTargetSpells with FastAttackAir with FastAttackGround {
  override type CasterType = CanUseStimpack
  override val spells = List(Spells.Stimpack)
}
