package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait VirtualHitPoints extends CanDie with Virtual {

  case class HitpointsSnapshot(hitPoints: HitPoints)

  private var lastSeen = Option.empty[HitpointsSnapshot]

  override def hitPoints = lastSeen.map(_.hitPoints).getOrElse(super.hitPoints)

  override def remember_!() = {
    super.remember_!()
    if (isEnemy) {
      lastSeen = HitpointsSnapshot(hitPoints).toSome
    }
  }

  override def forget_!() = {
    super.forget_!()
    lastSeen = None
  }

}
