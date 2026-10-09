package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait CanDetectHidden extends WrapsUnit with Detector {
  private val sight            = math.round(nativeUnitType.sightRange() / 32.0).toInt
  override def detectionRadius = sight
}
