package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Vulture(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundWeapon with HasSpiderMines with MediumAttackGround with Mechanic with MobileRangeWeapon with IsMedium with IsVehicle with ConcussiveGroundDamage with HasSinglePointMagicSpell with ArmedMobile {
  override type Caster = Vulture
  override val spells = List(Upgrades.Terran.SpiderMines)
}
