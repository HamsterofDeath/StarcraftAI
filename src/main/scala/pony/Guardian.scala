package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Guardian(unit: APIUnit)
    extends AnyUnit(unit) with ZergMobileUnit with AirUnit with GroundWeapon with IsBig with NormalGroundDamage
    with ArmedMobile with MediumAttackGround
