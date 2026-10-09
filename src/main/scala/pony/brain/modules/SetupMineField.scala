package pony
package brain
package modules

import bwapi.Color
import pony.Upgrades.Terran._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class SetupMineField(universe: Universe) extends DefaultBehaviour[Vulture](universe) {
  private val beLazy       = Idle -> Nil
  private val helper       = new FormationAtFrontLineHelper(universe, 2)
  private val plannedDrops = ArrayBuffer.empty[(Area, Int)]
  private val mined        = oncePerTick {
    plannedDrops.retain(_._2 + 120 > universe.currentTick)
    val area = universe.mapLayers.freeWalkableTiles.mutableCopy
    universe.ownUnits.allByType[SpiderMine].foreach { mine =>
      area.block_!(mine.blockedArea.extendedBy(1))
      plannedDrops.foreach(e => area.block_!(e._1))
    }
    area
  }

  override def renderDebug_!(renderer: Renderer): Unit = {
    suggestMinePositions.foreach { tile =>
      renderer.in_!(Color.White).drawCircleAroundTile(tile)
    }
  }

  private def suggestMinePositions = {
    val defense               = helper.allOutsideNonBlacklisted
    val neutralResourceFields = universe.resourceFields.resourceAreas.filterNot { field =>
      universe.bases.isCovered(field)
    }.map { _.nearbyFreeTile }
    // The carpet spreads vulture mines over every neutral field instead of only the front line.
    if (universe.strategy.current.usesCarpet) defense ++ neutralResourceFields
    else defense
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    ifNth(Primes.prime137) {
      helper.cleanBlacklist((_, reason) => reason.when + 240 < universe.currentTick)
    }
  }

  override def priority = SecondPriority.EvenLess

  override protected def wrapBase(unit: Vulture) = {
    // TODO calculate in background
    new SingleUnitBehaviour[Vulture](unit, meta) {

      private var state: State            = Idle
      private var originalSpiderMineCount = this.unit.spiderMineCount

      def freeArea = mined.get

      private var inBattle = false

      override def priority = if (inBattle) SecondPriority.EvenMore else super.priority

      override def describeShort: String = "Minefield"

      override def toOrder(what: Objective) = {
        val (newState, orders) = state match {
          case Idle =>
            // TODO include test in tech trait
            if (this.unit.spiderMineCount > 0 && this.unit.canCastNow(SpiderMines)) {
              val enemies = this.universe.unitGrid.enemy.allInRange[GroundUnit](this.unit.currentTile, 5)
              if (enemies.nonEmpty) {
                inBattle = true
                // drop mines on sight of enemy
                val on         = freeArea
                val freeTarget = {
                  on.spiralAround(this.unit.currentTile).filter(on.free)
                    .maxByOpt { where =>
                      def ownUnitsCost = {
                        this.universe.unitGrid.own.allInRange[GroundUnit](where, 5)
                          .view
                          .filter { e =>
                            !e.isInstanceOf[HasSpiderMines] && !e.isAutoPilot
                          }
                          .map(_.buildPrice)
                          .fold(Price.zero)(_ + _)
                      }
                      def enemyUnitsCost = {
                        this.universe.unitGrid.enemy.allInRange[GroundUnit](where, 5)
                          .view
                          .filter { e =>
                            !e.isInstanceOf[HasSpiderMines] && !e.isAutoPilot
                          }
                          .map(_.buildPrice)
                          .fold(Price.zero)(_ + _)
                      }
                      enemyUnitsCost - ownUnitsCost
                    }
                }
                freeTarget.map { where =>
                  on.block_!(where.asArea.extendedBy(1))
                  plannedDrops += where.asArea.extendedBy(1) -> this.universe.currentTick
                  DroppingMine(where)                        -> this.unit.toOrder(SpiderMines, where).toList
                }.getOrElse(beLazy)
              } else {
                inBattle = false
                // place mines on strategic positions
                val candiates    = suggestMinePositions
                val dropMineHere = candiates.minByOpt(_.distanceSquaredTo(this.unit.currentTile))
                dropMineHere.foreach(helper.blackList_!)
                dropMineHere.map { where =>
                  DroppingMine(where) -> this.unit.toOrder(SpiderMines, where).toList
                }.getOrElse(beLazy)
              }
            } else {
              inBattle = false
              beLazy
            }

          case myState @ DroppingMine(where) if this.unit.canCastNow(SpiderMines) =>
            myState -> this.unit.toOrder(SpiderMines, where).toList
          case DroppingMine(_) if this.unit.spiderMineCount < originalSpiderMineCount =>
            inBattle = false
            originalSpiderMineCount = this.unit.spiderMineCount
            Idle -> Nil
          case myState @ DroppingMine(_) =>
            inBattle = false
            myState -> Nil
        }
        state = newState
        orders
      }

      override def preconditionOk: Boolean = {
        this.universe.upgrades.hasResearched(Upgrades.Terran.SpiderMines)
      }
    }
  }

  trait State

  case class DroppingMine(tile: MapTilePosition) extends State

  case object Idle extends State

}
