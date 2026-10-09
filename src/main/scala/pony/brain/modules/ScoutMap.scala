package pony
package brain
package modules

import bwapi.Color

import scala.collection.mutable
import scala.reflect.ClassTag

class ScoutMap(universe: Universe) extends DefaultBehaviour[ArmedMobile](universe) {

  override def renderDebug_!(renderer: Renderer) = {
    super.renderDebug_!(renderer)
    renderer.in_!(Color.Blue)
    plan.plans.foreach { plan =>
      plan.covered.grouped(2).filter(_.size == 2).foreach { seq =>
        val List(a, b) = seq.toList
        renderer.drawLine(a.nearbyFreeTile, b.nearbyFreeTile)
        if (plan.nextTarget.contains(a.nearbyFreeTile)) {
          renderer.drawLine(a.nearbyFreeTile, plan.scouter.currentTile)
        } else if (plan.nextTarget.contains(b.nearbyFreeTile)) {
          renderer.drawLine(b.nearbyFreeTile, plan.scouter.currentTile)
        } else {
          // nop
        }
      }
    }
  }

  class ScoutPlan(scout: ArmedMobile, toCheck: List[ResourceArea]) {
    def scouter = scout

    def valid = {
      // While the enemy is unfound, a minimal scout must accept covered ground: the enemy main
      // is dangerous by definition, and refusing it would make discovery impossible.
      val seekingEnemy = !universe.pluginByType[RunTerranCampaign].enemyLocated
      scout.isInGame && toCheck.forall { resourceArea =>
        (seekingEnemy || mapLayers.dangerousAsBlocked.freeAndInBounds(resourceArea.nearbyFreeTile)) &&
        !bases.isCovered(resourceArea)
      }
    }

    val covered = toCheck.toSet

    private val remainingToCheck = mutable.ArrayBuffer.empty ++= toCheck.map(_.nearbyFreeTile)

    private val nextPath = {
      // The plain ground pathfinder is used only until the enemy is found: the safe variant
      // refuses to route into ground the enemy covers, which is exactly where the enemy main is.
      val pathfinder =
        if (universe.pluginByType[RunTerranCampaign].enemyLocated) universe.pathfinders.safeFor(scout)
        else universe.pathfinders.ground
      FutureIterator
        .feed((scout.currentTile, remainingToCheck.head, pathfinder))
        .produceAsync { case (from, to, pathfinder) =>
          pathfinder.findUnclampedPathNow(from, to).map(_.toMigration(using universe))
        }.named("Single scout plan")
    }

    private var cycle = 0

    def nextTarget = nextPath.flatMap { e =>
      e.map(_.originalDestination)
    }

    def onTick_!() = {
      nextPath.foreach(_.foreach(_.onTick_!()))
    }

    def toOrder = {
      nextPath.flatMap {
        case Some(paths) =>
          val close = scout.currentTile.distanceToIsLess(paths.originalDestination, 7) &&
            nativeGame.isVisible(paths.originalDestination.asTilePosition)
          if (close) {
            if (remainingToCheck.size > 1) {
              remainingToCheck.remove(0)
              nextPath.prepareNextIfDone()
            } else {
              val nextToCheckInOrder = {
                (if (cycle % 2 == 0) {
                   toCheck.reverse
                 } else {
                   toCheck
                 }).map(_.nearbyFreeTile)
              }
              remainingToCheck.remove(0)
              remainingToCheck ++= nextToCheckInOrder
              nextPath.prepareNextIfDone()
              cycle += 1
            }
            None
          } else {
            paths.nextPositionFor(scout) match {
              case None =>
                None
              case Some(to) =>
                Orders.MoveToTile(scout, to).toSome
            }
          }
        case None =>
          None
      }
    }
  }

  class Scouting {
    private val scouts = mutable.HashMap.empty[ArmedMobile, ScoutPlan]

    private val coveredRightNow = LazyVal.from {
      scouts.flatMap(_._2.covered).toSet
    }

    def plans = scouts.values

    case class ScoutingCandidate(id: Int, speed: Double, area: Grid2D, tile: MapTilePosition)

    case class RawScoutingPlan(
        sc: ScoutingCandidate,
        resourceAreaIds: List[Int],
        startHere: Int
    ) {
      def resourceAreaIdsInOrder = startHere :: (resourceAreaIds filterNot (_ == startHere))
    }

    class Feed {
      private val resourceAreasUnscouted = {
        val seekingEnemy = !universe.pluginByType[RunTerranCampaign].enemyLocated
        strategicMap.resources
          .filter { ra =>
            seekingEnemy || mapLayers.dangerousAsBlocked.free(ra.nearbyFreeTile)
          }
          .filterNot(coveredRightNow)
          .filterNot(bases.isCovered)
          .map { ra =>
            ra.uniqueId -> ra.nearbyFreeTile
          }
      }

      val tileToResourceAreaId = resourceAreasUnscouted.map(_.swap).toMap

      NativeMatchEvidence.trace(
        "scout-feed",
        s"areas=${resourceAreasUnscouted.map(_._1).mkString(",")} all=${strategicMap.resources.map(_.uniqueId).mkString(",")}"
      )

      val leftToCover        = resourceAreasUnscouted.map(_._2)
      val map                = mapLayers.rawWalkableMap
      val scoutingCandidates = {
        ownUnits.allMobilesWithWeapons
          .flatMap(_.asGroundUnit)
          .filter(e =>
            e.onGround && e.isInGame && !e.isBeingCreated &&
              !e.isInstanceOf[WorkerUnit] && !e.isInstanceOf[SupportUnit] && !e.isInstanceOf[TransporterUnit]
          )
          .filterNot(universe.pluginByType[RunTerranCampaign].isReservedDefender)
          .map { e =>
            ScoutingCandidate(
              e.nativeUnitId,
              e.initialNativeType.topSpeed(),
              e.currentArea.get,
              e.currentTile
            )
          }
      }

      val pathfinder = pathfinders.groundSafe
    }

    private val leftToCover = FutureIterator.feed(new Feed).produceAsyncLater { in =>
      val candidates = in.scoutingCandidates.groupBy(_.area).mapValuesStrict { who =>
        val sorted = who.toVector.sortBy(-_.speed)
        sorted.filter(_.speed == sorted.head.speed)
      }
      val coverUs = in.leftToCover.groupBy(in.map.getAreaOf)
      candidates.flatMap { case (where, who) =>
        coverUs.get(where).map { pointsToCheck =>
          val coveredAlready = mutable.HashSet.empty[MapTilePosition]

          def findNextBestPairAndScouter() = {
            val bestPair = ScoutPointPairs.next(pointsToCheck.toVector, coveredAlready.toSet) { (a, b) =>
              in.pathfinder.findSimplePathNow(a, b).map(_.length)
            }

            val bestScouter = {
              who.iterator
                .map { groundUnit =>
                  val bestStartingPoint = {
                    bestPair.map { tile =>
                      tile -> in.pathfinder.findSimplePathNow(groundUnit.tile, tile).map(_.length)
                    }.filter(_._2.isDefined)
                      .map { case (a, b) => a -> b.get }
                      .minByOpt(_._2)
                  }
                  bestStartingPoint.map { e =>
                    (groundUnit, e._1, e._2)
                  }
                }
                .flatten
                .minByOpt(_._3)
                .map(e => e._1 -> e._2)
            }
            bestScouter.foreach { _ =>
              coveredAlready ++= bestPair
            }
            bestScouter.map { bs =>
              RawScoutingPlan(
                bs._1,
                bestPair.map(in.tileToResourceAreaId),
                in.tileToResourceAreaId(bs._2)
              )
            }
          }
          Iterator.continually(findNextBestPairAndScouter()).takeWhile(_.isDefined).map(_.get)
            .toList

        }.getOrElse(Nil)
      }.toList
    }.named("Evaluate scouting plans")

    def onTick_!(): Unit = {
      if (race.isTerran && !universe.pluginByType[RunTerranCampaign].scoutingAllowed) return
      val oldSize = scouts.size
      scouts.filterInPlace { (_, v) => v.valid }
      if (oldSize != scouts.size) {
        coveredRightNow.invalidate()
      }

      scouts.valuesIterator.foreach(_.onTick_!())

      leftToCover.onceIfDone { plans =>
        val minimal    = race.isTerran && !universe.pluginByType[RunTerranCampaign].reconnaissanceAllowed
        val minimalCap = TerranCampaignConfig.load().minScouts
        val allowed    = if (minimal) math.max(0, minimalCap - scouts.size) else Int.MaxValue
        NativeMatchEvidence.trace(
          "scout-plan-options",
          s"minimal=$minimal allowed=$allowed plans=${plans.map(p =>
              s"${p.sc.id}:${p.resourceAreaIdsInOrder.mkString("/")}"
            ).mkString(" ")}"
        )
        // A minimal scout exists to find the enemy; visiting same-region area pairs would never
        // cross into the enemy's region. Instead it tours every unvisited field, farthest first.
        val chosen: List[RawScoutingPlan] = if (minimal) {
          val tour: List[Int] = if (scouts.nonEmpty) Nil
          else {
            val covered = coveredRightNow.get
            val home    = bases.mainBase.map(_.mainBuilding.tilePosition)
            strategicMap.resources
              .filterNot(bases.isCovered)
              .filterNot(covered)
              .toVector.sortBy(ra => home.map(h => -ra.nearbyFreeTile.distanceSquaredTo(h)).getOrElse(0))
              .map(_.uniqueId).toList
          }
          val scout = if (tour.isEmpty) None
          else ownUnits.allMobilesWithWeapons
            .flatMap(_.asGroundUnit)
            .filter(e =>
              e.onGround && e.isInGame && !e.isBeingCreated &&
                !e.isInstanceOf[WorkerUnit] && !e.isInstanceOf[SupportUnit] && !e.isInstanceOf[TransporterUnit]
            )
            .filterNot(universe.pluginByType[RunTerranCampaign].isReservedDefender)
            .toVector.sortBy(-_.initialNativeType.topSpeed()).headOption
          for {
            unit  <- scout.toList
            start <- tour.headOption.toList
          } yield RawScoutingPlan(
            ScoutingCandidate(unit.nativeUnitId, 0, unit.currentArea.get, unit.currentTile),
            tour.drop(1),
            start
          )
        } else plans.take(allowed)
        chosen.groupBy(_.sc.id).foreach { case (id, unitPlans) =>
          ownUnits.byId(id).foreach { stillLiving =>
            val ordered = unitPlans.flatMap(_.resourceAreaIdsInOrder).distinct
            ordered.headOption.foreach { start =>
              val resourceAreas = ordered.map(strategicMap.resourceAreaById)
              val unit          = stillLiving.asInstanceOf[ArmedMobile]
              scouts.put(unit, new ScoutPlan(unit, resourceAreas))
              NativeMatchEvidence.trace(
                "scout-plan",
                s"unit=$id minimal=$minimal start=$start order=${ordered.mkString(",")}"
              )
            }
          }
        }
        if (plans.nonEmpty || chosen.nonEmpty) {
          coveredRightNow.invalidate()
        }
      }
      ifNth(Primes.prime149) {
        leftToCover.prepareNextIfDone()
      }
    }

    // Asked for every unit every frame: units without a plan, nearly all of them, are answered first.
    def planFor(am: ArmedMobile) = scouts.get(am).filter { _ =>
      !am.isInstanceOf[WorkerUnit] && !universe.pluginByType[RunTerranCampaign].isReservedDefender(am) &&
      (if (race.isTerran) universe.pluginByType[RunTerranCampaign].scoutingAllowed
       else time.phase.isSinceAlmostMid)
    }
  }

  private val plan = new Scouting

  override def onTick_!() = {
    super.onTick_!()
    plan.onTick_!()
  }

  override def priority = SecondPriority.BetterThanNothing

  override protected def wrapBase(t: ArmedMobile) =
    new SingleUnitBehaviour[ArmedMobile](t, meta) {
      override def describeShort = "Scout"

      override protected def toOrder(what: Objective) = {
        plan.planFor(t).flatMap(_.toOrder).toList
      }
    }
}
