package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait StaticallyPositioned extends WrapsUnit {

  self =>

  val myTilePosition = once {
    val position = nativeUnit.getTilePosition
    val x = position.getX
    val y = position.getY
    MapTilePosition.shared(x, y)
  }

  val size = Size.shared(nativeUnit.getType.tileWidth(), nativeUnit.getType.tileHeight())

  private val myArea = once {
    Area(tilePosition, size)
  }

  private val myAreaOnMap = once {
    mapLayers.rawWalkableMap.areaOf(centerTile)
    .getOr(s"Building is not on valid ground: $self")
  }

  def areaOnMap = myAreaOnMap.get

  def nativeMapPosition = tilePosition.asMapPosition.toNative

  def tilePosition = myTilePosition.get

  /** Recompute the static position cells, e.g. after a building has flown and landed. */
  protected def refreshPositionCaches(): Unit = {
    myTilePosition.invalidate()
    myArea.invalidate()
    myAreaOnMap.invalidate()
  }

  override def center = area.center

  def area = myArea.get

  override def toString: String = {
    s"${super.toString}@${area.describe}"
  }
}
