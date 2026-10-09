package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

sealed class DamageFactor(val factor: Int)

case object Full extends DamageFactor(100)

case object ThreeQuarters extends DamageFactor(75)

case object Half extends DamageFactor(50)

case object Quarter extends DamageFactor(25)

case object Zero extends DamageFactor(0)
