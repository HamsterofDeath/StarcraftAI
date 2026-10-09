package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

case class Circle(center: MapTilePosition, radius: Int, maxX: Int, maxY: Int) {
  def asTiles = new GeometryHelpers(maxX, maxY).tilesInCircle(center, radius)
}
