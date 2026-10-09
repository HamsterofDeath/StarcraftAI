package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait VirtualCloakHelpers
  extends CanCloak with Virtual with VirtualCloak with VirtualHitPoints with VirtualPosition {

  override def onTick_!() = {
    super.onTick_!()
    if (isEnemy) {
      if (age == 0) {
        remember_!()
      } else if (isExposed) {
        update_!()
      }
    }
  }
}
