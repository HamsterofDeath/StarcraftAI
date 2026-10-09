package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Hydralisk(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with ZergMobileUnit with ExplosiveAirDamage
    with ArmedMobile with ExplosiveGroundDamage with IsMedium with FastAttackAir with FastAttackGround with CanBurrow
    with Virtual
