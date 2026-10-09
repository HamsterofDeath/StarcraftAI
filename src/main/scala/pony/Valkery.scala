package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Valkery(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with MobileRangeWeapon with IsBig with IsShip
    with ArmedMobile with BadDancer with ExplosiveAirDamage with SlowAttackAir
