package pony
package brain
package modules

import bwapi.Color

import scala.reflect.ClassTag

class Dance(universe: Universe) extends DefaultBehaviour[ArmedMobile](universe) {

  override def renderDebug_!(renderer: Renderer) = {
    super.renderDebug_!(renderer)
    dancePlan.foreach { plan =>
      plan.foreach { case (unitId, goto) =>
        ownUnits.byId(unitId).foreach { unit =>
          renderer.in_!(Color.Yellow).drawLine(unit.center, goto.asMapPosition)
        }
      }
    }
  }

  private val freeWalkableMap = oncePerTick {
    mapLayers.freeWalkableTiles.guaranteeImmutability
  }
  private val safeFromLongRange = oncePerTick {
    mapLayers.coveredByEnemyLongRangeGroundAsBlocked.guaranteeImmutability
  }

  private val dropAtFirst = 36
  private val tries       = 100
  private val range       = 10
  private val takeNth     = 7
  private val dancePlan   = FutureIterator.feed(feed).produceAsyncLater { in =>
    val on          = in.freeMap
    val safetyCheck = in.safeFromLongRange
    in.dancers.map { dancer =>
      val center = MapTilePosition.average(dancer.partners.iterator.map(_.where))

      val myArea    = universe.mapLayers.rawWalkableMap.areaOf(dancer.me.where)
      val danceMove = {
        myArea.flatMap { a =>
          def tileIterator = {
            on.spiralAround(dancer.me.where, range)
              .drop(dropAtFirst)
              .sliding(1, takeNth)
              .flatten
              .take(tries)
          }
          def antiGravTry = {
            var xSum = 0
            var ySum = 0

            for (dp <- dancer.partners) {
              val diff = dp.where.diffTo(dancer.me.where)
              xSum += diff.x
              ySum += diff.y
            }

            val ref = dancer.me.where.movedBy(MapTilePosition(xSum, ySum))

            tileIterator.filter { c =>
              c.distanceSquaredTo(ref) <= dancer.me.where.distanceSquaredTo(center)
            }.find(e => on.free(e) && a.free(e) && safetyCheck.free(e))
          }

          def secondTry = {
            tileIterator
              .filter(e => on.free(e) && a.free(e) && safetyCheck.free(e))
              .maxByOpt(center.distanceSquaredTo)
          }

          antiGravTry.orElse(secondTry)
        }
      }
      dancer -> danceMove
    }.collect {
      case (k, v) if v.isDefined =>
        k.me.id -> v.get
    }.toMap
  }.named("Dance plan")

  override def priority: SecondPriority = SecondPriority.More

  override def onTick_!() = {
    super.onTick_!()
    ifNth(Primes.prime5) {
      dancePlan.prepareNextIfDone()
    }
  }

  override protected def wrapBase(unit: ArmedMobile): SingleUnitBehaviour[ArmedMobile] =
    new SingleUnitBehaviour[ArmedMobile](unit, meta) {

      trait State

      case object Idle extends State

      case class Fallback(to: MapTilePosition, startedAtTick: Int) extends State

      private var state: State    = Idle
      private var runningCommands = List.empty[UnitOrder]
      private val noop            = (Idle, List.empty[UnitOrder])

      override def describeShort: String = "Dance"

      override def toOrder(what: Objective) = {
        val (newState, newOrder) = {
          val canDance = !this.unit.isInstanceOf[BadDancer] || this.unit.hasBeenAttackedSince(8)
          if (this.unit.isReadyToFireWeapon || !canDance) {
            noop
          } else {
            state match {
              case Idle =>
                dancePlan.flatMapOnContent { plan =>
                  plan.get(this.unit.nativeUnitId)
                }.map { where =>
                  Fallback(where, this.universe.currentTick) -> Orders.MoveToTile(this.unit, where).toList
                }.getOrElse(noop)
              case current @ Fallback(where, startedWhen) =>
                if (this.unit.isReadyToFireWeapon || this.unit.currentTile == where) {
                  noop
                } else {
                  (current, runningCommands)
                }
            }
          }
        }
        state = newState
        runningCommands = newOrder
        runningCommands
      }
    }

  private def feed = {
    val dancers = {
      ownUnits.allMobilesWithWeapons
        .flatMap { own =>
          val dancePartners = {
            def take(enemy: ArmedMobile) = {
              (own.isInstantFireUnit ||
                enemy.initialNativeType.topSpeed <= own.initialNativeType.topSpeed) &&
              enemy.weaponRangeRadius <= own.weaponRangeRadius &&
              own.canAttackIfNear(enemy)
            }
            unitGrid.allInRangeOf[ArmedMobile](
              own.currentTile,
              own.weaponRangeRadiusTiles,
              friendly = false,
              take
            ).map { unit =>
              UnitIdPosition(unit.nativeUnitId, unit.currentTile)
            }
          }

          if (dancePartners.nonEmpty) {
            Dancer(UnitIdPosition(own.nativeUnitId, own.currentTile), dancePartners.toVector)
              .toSome
          } else {
            None
          }
        }.toVector
    }
    Feed(dancers, freeWalkableMap, safeFromLongRange)
  }

  case class UnitIdPosition(id: Int, where: MapTilePosition)

  case class Dancer(me: UnitIdPosition, partners: Seq[UnitIdPosition])

  case class Feed(dancers: Seq[Dancer], freeMap: Grid2D, safeFromLongRange: Grid2D)

  case class DancePlan(data: Map[ArmedMobile, MapTilePosition])

}
