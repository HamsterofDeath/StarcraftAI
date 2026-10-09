package pony

import pony.brain.modules.GroupingHelper

import scala.collection.mutable.ArrayBuffer

class ResourceAnalyzer(map: AnalyzedMap, all: AllUnits) {

  lazy val groups = myGroups.zipWithIndex.map { case (serializable, index) =>
    val pg = new MineralPatchGroup(index)
    serializable.areas.foreach { e =>
      val minerals = myUnits.minerals.find(_.area == e).getOr(s"Could not find minerals at $e")
      pg.addPatch(minerals)
    }
    pg
  }
  lazy val resourceAreas = {
    val mineralBased = groups.map { patchGroup =>
      val geysirs = myUnits.geysirs
        .filter { e =>
          val close    = e.area.distanceTo(patchGroup.center) < 20
          def sameArea = patchGroup.patches.exists { p =>
            map.walkableGrid.areInSameWalkableArea(e.centerTile, p.centerTile)
          }
          close && sameArea
        }
        .toSet

      ResourceArea(Some(patchGroup), geysirs)
    }
    val allAreas = mineralBased ++ {
      val lonelyGeysirs = myUnits.geysirs
        .filterNot { geysir =>
          mineralBased.exists(_.geysirs(geysir))
        }
      val groups = GroupingHelper.groupTheseNow(lonelyGeysirs, map.walkableGrid, all)
      groups.map { group =>
        ResourceArea(None, group.memberUnits.toSet)
      }
    }
    allAreas.toVector
  }

  val myUnits          = all.own
  private val myGroups = FileStorageLazyVal.fromFunction(
    {
      info(s"Calculating mineral groups...")
      val patchGroups = ArrayBuffer.empty[MineralPatchGroup]
      val allMins     = ArrayBuffer.empty ++= myUnits.minerals
      val pathFinder  = PathFinder.on(map.walkableGrid, isOnGround = true)
      allMins.foreach { mp =>
        print(".")
        patchGroups.find { g =>
          def isNew   = !g.contains(mp)
          def isClose = {
            g.allTiles.exists { check =>
              val path = pathFinder.findSimplePathNow(check, mp.tilePosition, tryFixPath = false)
              path.exists { p =>
                p.isPerfectSolution && p.length < 20
              }
            }
          }
          isNew && isClose
        } match {
          case Some(group) => group.addPatch(mp)
          case None        =>
            val newGroup = new MineralPatchGroup(patchGroups.size)
            newGroup.addPatch(mp)
            patchGroups += newGroup
        }
      }

      patchGroups.map(e => SerializablePatchGroup(e.patches.map(_.area).toSeq))
    },
    s"mineralgroups_${map.game.suggestFileName}"
  )

  def nearestTo(position: MapTilePosition) = {
    if (groups.nonEmpty)
      Some(groups.minBy(_.center.distanceTo(position)))
    else
      None
  }

  info(
    s"""
       |Detected ${groups.size} mineral groups
     """.stripMargin
  )
}
