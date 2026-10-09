package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait Virtual extends CanDie {

  def remember_!(): Unit = {}

  def forget_!(): Unit = {}

  def update_!(): Unit = {
    forget_!()
    remember_!()
  }

}
