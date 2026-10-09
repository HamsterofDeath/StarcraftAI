package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Corsair(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with IsMedium with IsShip with ExplosiveAirDamage
    with ArmedMobile with FastAttackAir
