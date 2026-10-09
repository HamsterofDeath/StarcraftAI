package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

case class DamageSingleAttack(onHp: Int, onShields: Int, airHit: Boolean) extends HasHpAndShields {
  override def hp: Int = onHp

  override def shields: Int = onShields
}
