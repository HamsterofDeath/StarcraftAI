package pony

import bwapi.Game

import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.language.postfixOps

class AnalyzedMap(val game: Game) {

  val sizeX = game.mapWidth() * 4
  val sizeY = game.mapHeight() * 4

  val tileSizeX = game.mapWidth()
  val tileSizeY = game.mapHeight()

  val empty       = new Grid2D(sizeX, sizeY, BitSet.empty)
  val emptyZoomed = empty.zoomedOut

  val walkableGridZoomed = {
    val bits = mutable.BitSet.empty
    0 until sizeX map { x =>
      0 until sizeY map { y =>
        if (!game.isWalkable(x, y)) {
          bits += x + (sizeX * y)
        }
      }
    }
    new Grid2D(sizeX, sizeY, bits).minAreaSize(6)
  }

  val buildableGrid = {
    val bits = mutable.BitSet.empty
    0 until sizeX / 4 map { x =>
      0 until sizeY / 4 map { y =>
        if (!game.isBuildable(x, y)) {
          bits += x + (sizeX / 4 * y)
        }
      }
    }
    new Grid2D(sizeX / 4, sizeY / 4, bits).minAreaSize(6)
  }

  val buildableGridZoomed = {
    // fake this for math reasons
    val bits = mutable.BitSet.empty
    0 until sizeX map { x =>
      0 until sizeY map { y =>
        if (!buildableGrid.free(x / 4, y / 4)) {
          bits += x + (sizeX * y)
        }
      }
    }
    new Grid2D(sizeX, sizeY, bits)
  }

  val walkableGrid = walkableGridZoomed.zoomedOut

  val areas = walkableGrid.areas

  def debugAreas = {
    val encoded   = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ (1 to 100 map (_ => '?'))
    val areas     = walkableGrid.areas
    val separated = areas.map(_.mkString('X')).mkString("\n---\n")
    val debugThis = walkableGrid
    separated + "\n" +
      (0 until debugThis.rows map { y =>
        0 until debugThis.cols map { x =>
          val index = areas.indexWhere(_.free(x, y))
          if (index == -1) " " else encoded(index).toString
        } mkString
      } mkString "\n")
  }

  def debugMap = walkableGrid.mkString('X')

  def debugMap2 = buildableGrid.mkString('X')

  info(
    s"""
       |Received map ${game.mapName()} with hash ${game.mapHash()}, size $sizeX * $sizeY
       |Total tiles ${walkableGridZoomed.size}
       |Walkable tiles ${walkableGridZoomed.walkable}
       |Blocked tiles ${walkableGridZoomed.blocked}
       |Walkable map
       |$debugMap
       |Buildable map
       |$debugMap2
       |Area analysis
       |$debugAreas
     """.stripMargin
  )
}
