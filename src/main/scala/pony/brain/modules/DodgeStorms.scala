package pony
package brain
package modules

import scala.jdk.CollectionConverters._

/**
  * A unit in or next to a psionic storm leaves it at once, straight away from the storm's centre, above every other
  * behaviour. A sieged tank cannot move in time and stays.
  */
class DodgeStorms(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
  private var storms = Vector.empty[(Double, Double)]

  override def priority = SecondPriority.Max

  override def onTick_!(): Unit = {
    super.onTick_!()
    storms = nativeGame.getBullets.asScala.iterator
      .filter(b => b.exists && b.getType == bwapi.BulletType.Psionic_Storm)
      .map(b => (b.getPosition.getX.toDouble, b.getPosition.getY.toDouble))
      .toVector
  }

  override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {
    override def describeShort = "Dodge storm"

    override protected def toOrder(what: Objective) = {
      val me     = this.unit
      val native = me.nativeUnit
      val sieged = me match {
        case t: Tank => t.isSieged
        case _       => false
      }
      if (storms.isEmpty || sieged || !native.exists) Nil
      else
        StormDodge.escape((native.getX.toDouble, native.getY.toDouble), storms, native.isUnderStorm).map { (x, y) =>
          val grid = mapLayers.rawWalkableMap
          val tile = MapTilePosition.shared(
            (x / 32).toInt.max(0).min(grid.cols - 1),
            (y / 32).toInt.max(0).min(grid.rows - 1)
          )
          Orders.MoveToTile(me, tile)
        }.toList
    }
  }
}

/** Where to flee a storm, kept free of the game; pixels. */
private[pony] object StormDodge {

  /** A storm covers about three tiles square; this close to its centre a unit is in it or about to be. */
  val Reach = 80.0

  /** How far a fleeing unit goes from the storm's centre. */
  val Escape = 128.0

  def escape(at: (Double, Double), storms: Seq[(Double, Double)], underStorm: Boolean): Option[(Double, Double)] = {
    val near = storms.filter(s => math.hypot(at._1 - s._1, at._2 - s._2) < Reach)
    if (near.isEmpty && !underStorm) None
    else {
      val (sx, sy) =
        if (near.nonEmpty) (near.map(_._1).sum / near.size, near.map(_._2).sum / near.size)
        else storms.minBy(s => math.hypot(at._1 - s._1, at._2 - s._2))
      val (dx, dy) = (at._1 - sx, at._2 - sy)
      val length   = math.hypot(dx, dy)
      val (ux, uy) = if (length < 1) (1.0, 0.0) else (dx / length, dy / length)
      Some((sx + ux * Escape, sy + uy * Escape))
    }
  }
}
