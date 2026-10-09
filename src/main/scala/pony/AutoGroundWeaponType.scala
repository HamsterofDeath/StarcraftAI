package pony

import bwapi.{Unit => APIUnit, DamageType => _, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait AutoGroundWeaponType extends GroundWeapon {
  override val groundDamageType = DamageTypes.fromNative(groundWeapon.damageType)
}
