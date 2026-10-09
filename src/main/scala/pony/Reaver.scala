package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Reaver(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with Mechanic with IsBig with IsVehicle with ArmedMobile
    with NormalGroundDamage with SlowAttackGround
