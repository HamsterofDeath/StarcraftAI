package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Mutalisk(unit: APIUnit)
  extends AnyUnit(unit) with ZergMobileUnit with AirUnit with GroundAndAirWeapon with NormalGroundDamage with ArmedMobile with NormalAirDamage with IsMedium with FastAttackAir with FastAttackGround
