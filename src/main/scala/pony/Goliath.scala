package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Goliath(unit: APIUnit)
  extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with FastAttackGround with SlowAttackAir with Mechanic with MobileRangeWeapon with IsBig with IsVehicle with NormalGroundDamage with BadDancer with ExplosiveAirDamage with ArmedMobile
