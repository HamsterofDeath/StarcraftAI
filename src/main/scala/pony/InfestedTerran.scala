package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class InfestedTerran(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with NormalGroundDamage with IsSmall with ArmedMobile
    with MeleeWeapon with FastAttackGround with ZergMobileUnit
