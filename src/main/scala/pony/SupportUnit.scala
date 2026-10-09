package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait SupportUnit extends Mobile {
  private val myNearestAlliesWithWeapons = oncePer(Primes.prime47) {
    ownUnits.allMobilesWithWeapons.iterator.toVector.sortBy { other =>
      other.centerTile.distanceSquaredTo(centerTile)
    }
  }

  def nearestAllies = myNearestAlliesWithWeapons.get
}
