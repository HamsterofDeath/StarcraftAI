package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

case class HitPoints(hitpoints: Int, shield: Int) {

  def isDead = hitpoints == 0

  def sum = hitpoints + shield

  def <=(other: HitPoints): Boolean = <=(other.hitpoints, other.shield)

  def <=(otherHp: Int, otherShield: Int) = {
    hitpoints <= otherHp || shield <= otherShield
  }

  def <(other: HitPoints): Boolean = <(other.hitpoints, other.shield)

  def <(otherHp: Int, otherShield: Int) = {
    hitpoints < otherHp || shield < otherShield
  }

  def <(t: (Int, Int)) = {
    hitpoints < t._1 || shield < t._2
  }
}
