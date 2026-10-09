package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Interceptor(unit: APIUnit)
  extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsSmall with IsShip with ArmedMobile with NormalAirDamage with NormalGroundDamage with FastAttackAir with FastAttackGround
