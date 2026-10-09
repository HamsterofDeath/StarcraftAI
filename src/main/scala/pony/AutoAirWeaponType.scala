package pony

import bwapi.{Unit => APIUnit, DamageType => _, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait AutoAirWeaponType extends AirWeapon {
  override val airDamageType = DamageTypes.fromNative(airWeapon.damageType)
}
