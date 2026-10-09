package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait VirtualCloak extends CanCloak with Virtual {

  case class CloakStateSnapshot(cloaked: Boolean)

  private var lastSeen = Option.empty[CloakStateSnapshot]
  override def isCloaked = lastSeen.map(_.cloaked).getOrElse(super.isCloaked)

  override def remember_!() = {
    super.remember_!()
    if (isEnemy) {
      lastSeen = CloakStateSnapshot(isCloaked).toSome
    }
  }

  override def forget_!() = {
    super.forget_!()
    lastSeen = None
  }
}
