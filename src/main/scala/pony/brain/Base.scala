package pony
package brain

import pony.terrain.ResourceArea
import pony.units.MainBuilding
import pony.util.BWFuture

case class Base(mainBuilding: MainBuilding) {

  def world = mainBuilding.world

  def resourceArea = {
    world.resourceAnalyzer.resourceAreas
      .minByOpt { c =>
        mainBuilding.area.distanceTo(c.center)
      }
  }

  /**
    * Whether the depot stands at its field. The field is merely the nearest one, so a depot built at home to fly out
    * later is bound to a field far away: in game 1 on 156cb30 one such depot sent miners and refinery builders into the
    * wall for 40 minutes.
    */
  def atField = resourceArea.exists(a => !mainBuilding.centerTile.distanceToIsMore(a.center, Base.FieldReach))

  // Bind resource geometry on the native callback thread before the alternative-path future starts.
  // The current field remains dynamic so a lifted depot can later rebind after landing.
  private val initialResourceArea = resourceArea

  def alternativeResourceAreas = myAlternativeResourceAreas.result

  private val myAlternativeResourceAreas = {
    val safeGround = mainBuilding.pathfinders.groundSafe
    val safeAir    = mainBuilding.pathfinders.airSafe
    val tile       = mainBuilding.centerTile

    def sortByPath = {

      def evaluate(area: ResourceArea, ground: Boolean) = {
        val finder = if (ground) safeGround else safeAir
        val path   = safeGround.findSimplePathNow(tile, area.anyTile)

        path match {
          case None =>
            None
          case Some(p) =>
            Some(area -> p.length)

        }
      }
      val ground = world.resourceAnalyzer.resourceAreas.flatMap(evaluate(_, true))

      val air = world.resourceAnalyzer.resourceAreas.filterNot { area =>
        ground.exists(_._1 == area)
      }.flatMap(evaluate(_, false))

      val all = ground.sortBy(_._2).map(_._1) ++ air.sortBy(_._2).map(_._1)
      all.filterNot(initialResourceArea.contains).filter { candidate =>
        mainBuilding.mapLayers.rawWalkableMap
          .areInSameWalkableArea(candidate.anyTile, tile)
      }
    }

    BWFuture(sortByPath, Nil)
  }

  def myMineralGroup = resourceArea.flatMap(_.patches)
  def myGeysirs      = resourceArea.map(_.geysirs).getOrElse(Set.empty)

  info(
    s"""
       |Found base/minerals $mainBuilding: $myMineralGroup
     """.stripMargin
  )

  override def toString: String = s"Base@$mainBuilding"
}

object Base {

  /** Tiles between a depot and its field's centre within which it mines the field. */
  val FieldReach = 12
}
