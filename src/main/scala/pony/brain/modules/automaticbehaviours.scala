package pony
package brain
package modules

import bwapi.Color
import pony.Upgrades.Terran._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class DefaultBehaviour[T <: WrapsUnit : Manifest](override val universe: Universe)
  extends HasUniverse {

  def employer = asEmployer

  private val asEmployer      = new Employer[T](universe)
  private val unit2behaviour  = mutable.HashMap.empty[T, SingleUnitBehaviour[T]]
  private val controlledUnits = mutable.HashSet.empty[T]

  override def onTick(): Unit = {
    super.onTick()
    val remove = controlledUnits.filterNot(_.isInGame)
    controlledUnits --= remove
    unit2behaviour --= remove

  }
  def renderDebug(renderer: Renderer): Unit = {}
  def behaviourOf(unit: T) = {
    ifControlsOpt(unit) {identity}
  }
  def ifControlsOpt[R](m: T)(f: (SingleUnitBehaviour[T]) => R) = {
    ifControls(m, Option.empty[R])(e => Some(f(e)))
  }
  def ifControls[R](m: T, ifNot: R)(f: (SingleUnitBehaviour[T]) => R) = {
    if (controls(m)) {
      f(unit2behaviour(assumeSafe(m)))
    } else {
      ifNot
    }
  }
  def controls(unit: T) = {
    canControl(unit) && controlledUnits.contains(assumeSafe(unit))
  }
  def canControl(u: WrapsUnit) = {
    manifest[T].runtimeClass.isInstance(u) && !u.isInstanceOf[AutoPilot]
  }
  def assumeSafe(unit: WrapsUnit): T = unit.asInstanceOf[T]
  def add_!(u: WrapsUnit, objective: Objective) = {
    assert(canControl(u))
    val behaviour = wrapBase(u.asInstanceOf[T])
    controlledUnits += behaviour.unit
    unit2behaviour.put(behaviour.unit, behaviour)
  }
  def cast = this.asInstanceOf[DefaultBehaviour[WrapsUnit]]
  protected def meta = SingleUnitBehaviourMeta(priority, refuseCommandsForTicks,
    forceRepeatedCommands)
  def forceRepeatedCommands = false
  def priority = SecondPriority.Default
  def refuseCommandsForTicks = 0
  protected def wrapBase(unit: T): SingleUnitBehaviour[T]

}

object Terran {
  def allBehaviours(universe: Universe): Seq[DefaultBehaviour[WrapsUnit]] = {
    val allOfThem = (
                      new StimSelf(universe) ::
                      new StopMechanic(universe) ::
                      new SetupMineField(universe) ::
                      new ShieldUnit(universe) ::
                      new IrradiateUnit(universe) ::
                      new CloakSelfGhost(universe) ::
                      new GoToInitialPosition(universe) ::
                      new CloakSelfWraith(universe) ::
                      new SiegeUnsiegeSelf(universe) ::
                      new MigrateTowardsPosition(universe) ::
                      new TransportGroundUnits(universe) ::
                      new RepairDamagedUnit(universe) ::
                      new RepairDamagedBuilding(universe) ::
                      new MoveAwayFromConstructionSite(universe) ::
                      new PreventBlockades(universe) ::
                      new ContinueInterruptedConstruction(universe) ::
                      new UseComsat(universe) ::

                      /*
                                            new RepairDamagedBuilding ::
                                           new Scout ::
                                           new DoNotStray ::
                                           new HealDamagedUnit ::
                                           new FixMedicalProblem ::
                                           new BlindDetector ::
                      */
                      new Dance(universe) ::
                      new FocusFire(universe) ::
                      new ReallyReallyLazy(universe) ::
                      Nil)
                    .map(_.cast)
    allOfThem
  }

  class TransportGroundUnits(universe: Universe)
    extends DefaultBehaviour[TransporterUnit](universe) {

    override def forceRepeatedCommands: Boolean = false

    private val timeBetweenUpdates = 24
    private val maxAge             = 24 * 4

    override protected def wrapBase(unit: TransporterUnit) = new
        SingleUnitBehaviour[TransporterUnit](unit, meta) {

      trait PositionOrUnit {
        def where: MapTilePosition
        def unit: Option[GroundUnit]
      }

      case class IsPosition(where: MapTilePosition) extends PositionOrUnit {
        override def unit = None
      }
      case class IsUnit(basedOn: GroundUnit) extends PositionOrUnit {
        override def where = basedOn.currentTile
        private val u = basedOn.toSome
        override def unit = u
      }
      case class MaybePath(path: FutureIterator[PositionOrUnit, Option[MigrationPath]]) {
        private var lastTouched = currentTick
        def age = currentTick - lastTouched

        def updateFuture(): Unit = {
          trace(s"Update requested for path to ${path.feedObj}")
          path.prepareNextIfDone()
          lastTouched = currentTick
        }
      }

      private val paths = mutable.HashMap.empty[PositionOrUnit, MaybePath]

      override def forceRepeats: Boolean = true

      override def blocksForTicks: Int = 24

      override def describeShort: String = "Transport"

      override def toOrder(what: Objective): Seq[UnitOrder] = {
        ifNth(Primes.prime61) {
          val old = paths.filter { case (_, maybe) => maybe.age > maxAge }
                    .keySet
                    .toList
          paths --= old
          trace(s"Kicked out obsolete paths for: ${old}")
        }

        def currentSafeOrder(transporterTarget: PositionOrUnit): Option[UnitOrder] = {
          val maybeCalculatedPath = paths.getOrElseUpdate(transporterTarget, {
            val future = FutureIterator.feed(transporterTarget).produceAsync { target =>
              val path = pathfinder.airSafe.findPathNow(unit.currentTile, target.where)
              path.map(e => new Paths(List(e))).map(new MigrationPath(_, universe, false))
            }
            MaybePath(future)
          })
          if (maybeCalculatedPath.age > timeBetweenUpdates) {
            maybeCalculatedPath.updateFuture()
          }
          maybeCalculatedPath.path.mostRecent.flatMap { maybeFoundPath =>
            maybeFoundPath.map { safePath =>
              def simpleCommand = {
                transporterTarget.unit match {
                  case Some(pickupTarget) =>
                    Orders.LoadUnit(unit, pickupTarget)
                  case None =>
                    Orders.MoveToTile(unit, transporterTarget.where)
                }
              }
              val near = unit.currentTile.distanceToIsLess(transporterTarget.where, 5)
              if (near)
                simpleCommand
              else
                safePath.nextFor(unit).map { case (where, _) =>
                  Orders.MoveToTile(unit, where)
                }.getOrElse(simpleCommand)
            }
          }
        }

        def orderByUnit(groundUnit: GroundUnit): Option[UnitOrder] = {
          val what = IsUnit(groundUnit)
          currentSafeOrder(what)
        }

        def orderByTile(simpleTile: MapTilePosition): Option[UnitOrder] = {
          val what = IsPosition(simpleTile)
          currentSafeOrder(what)
        }

        ferryManager.planFor(unit) match {
          case Some(plan) =>
            plan.toDropNow.map { dropThis =>
              Orders.UnloadUnit(unit, dropThis)
            }.orElse {
              if (plan.unloadedLeft) {
                val loadThis = plan.toTransport
                               .view
                               .filterNot(_.loaded)
                               .minBy(_.currentTile.distanceSquaredTo(unit.currentTile))
                orderByUnit(loadThis)
              } else if (plan.needsToReachTarget) {
                orderByTile(plan.toWhere)
              } else if (plan.loadedLeft) {
                plan.nextToDropAtTarget.map { drop =>
                  Orders.UnloadUnit(unit, drop)
                }
              } else {
                None
              }
            }.toList
          case None =>
            (if (unit.hasUnitsLoaded) {
              val nearestFree = mapLayers.freeWalkableTiles.nearestFree(unit.currentTile)
              nearestFree.map { where =>
                Orders.UnloadAll(unit, where).forceRepeat_!(true)
              }
            } else if (unit.isPickingUp) {
              Orders.Stop(unit).toSome
            }
            else {
              None
            }).toList
        }
      }
    }
  }

  class Dance(universe: Universe) extends DefaultBehaviour[ArmedMobile](universe) {

    private val immutableMapLayer = oncePerTick {
      mapLayers.freeWalkableTiles.guaranteeImmutability
    }
    override def priority: SecondPriority = SecondPriority.More
    override def canControl(u: WrapsUnit): Boolean = super.canControl(u) &&
                                                     !u.isInstanceOf[BadDancer]
    override def refuseCommandsForTicks = 6
    override protected def wrapBase(unit: ArmedMobile): SingleUnitBehaviour[ArmedMobile] = new
        SingleUnitBehaviour[ArmedMobile](unit, meta) {

      trait State
      case object Idle extends State
      case class Fallback(to: MapTilePosition, startedAtTick: Int) extends State

      private var state: State    = Idle
      private var runningCommands = List.empty[UnitOrder]
      private val beLazy          = (Idle, List.empty[UnitOrder])
      private val dropAtFirst     = 36
      private val tries           = 100
      private val range           = 10
      private val takeNth         = 7

      override def describeShort: String = "Dance"
      override def toOrder(what: Objective) = {
        val (newState, newOrder) = {
          state match {
            case Idle =>
              val coolingDown = !unit.isReadyToFireWeapon
              lazy val dancePartners = {
                val allEnemies = universe.unitGrid.enemy.allInRange[Mobile](unit.currentTile,
                  unit.weaponRangeRadius / 32)
                allEnemies
                .collect { case enemy: Weapon if
                enemy.initialNativeType.topSpeed <= unit.initialNativeType.topSpeed &&
                enemy.weaponRangeRadius <= unit.weaponRangeRadius &&
                unit.canAttack(enemy) =>
                  enemy
                }
              }
              if (coolingDown && dancePartners.nonEmpty) {
                val on = immutableMapLayer.get
                var xSum = 0
                var ySum = 0
                var xSumCenter = 0
                var ySumCenter = 0
                for (dp <- dancePartners) {
                  val diff = dp.currentTile.diffTo(unit.currentTile)
                  xSum += diff.x
                  ySum += diff.y
                  xSumCenter += dp.currentTile.x
                  ySumCenter += dp.currentTile.y
                }
                val ref = unit.currentTile.movedBy(MapTilePosition(xSum, ySum))
                val dpSize = dancePartners.size
                val whereToGo = {
                  // first try: just escape antigravity style
                  on.spiralAround(unit.currentTile, range)
                  .drop(dropAtFirst)
                  .sliding(1, takeNth)
                  .flatten
                  .take(tries)
                  .filter { c =>
                    c.distanceSquaredTo(ref) <= unit.currentTile.distanceSquaredTo(ref)
                  }.find {on.free}
                  .orElse {
                    // second try: consider slipping through enemy lines
                    val center = MapTilePosition(xSumCenter / dpSize, ySumCenter / dpSize)
                    val map = on
                    val myArea = map.areaWhichContainsAsFree(unit.currentTile)
                    myArea.flatMap { a =>
                      on.spiralAround(unit.currentTile, range)
                      .drop(dropAtFirst)
                      .sliding(1, takeNth)
                      .flatten
                      .take(tries)
                      .filter(map.free)
                      .filter(a.free)
                      .maxByOpt(center.distanceSquaredTo)
                    }
                  }
                }

                whereToGo.map { where =>
                  Fallback(where, universe.currentTick) -> Orders.MoveToTile(unit, where).toList
                }.getOrElse(beLazy)
              } else {
                beLazy
              }
            case current@Fallback(where, startedWhen) =>
              if (unit.isReadyToFireWeapon || startedWhen + 24 < universe.currentTick ||
                  unit.currentTile == where) {
                beLazy
              } else {
                (current, runningCommands)
              }
          }
        }
        state = newState
        runningCommands = newOrder
        runningCommands

      }
    }
  }

  class MoveAwayFromConstructionSite(universe: Universe)
    extends DefaultBehaviour[Mobile](universe) {
    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {
      private var lastTarget = Option.empty[MapTilePosition]

      override def describeShort: String = "<>"
      override def toOrder(what: Objective): Seq[UnitOrder] = {
        def secondLayer = mapLayers.freeWalkableTiles
        def layer = mapLayers.blockedByPlannedBuildings
        val extendedSizeToBeSafe = unit.unitTileSize.growBy(tolerance + 2)
        def shouldAttempt = {
          universe.unitGrid.own.allInRange[Mobile](unit.currentTile, 4).size < 12
        }
        val needsToMove = layer.anyBlocked(unit.blockedArea.growBy(tolerance)) && shouldAttempt

        val order = lastTarget.filter { where =>
          needsToMove &&
          layer.freeAndInBounds(where, extendedSizeToBeSafe) &&
          secondLayer.freeAndInBounds(where, extendedSizeToBeSafe)
        }.map { where =>
          Orders.MoveToTile(unit, where)
        }.orElse {
          if (needsToMove) {
            val myLayer = layer
            myLayer.spiralAround(unit.currentTile).find { tile =>
              myLayer.freeAndInBounds(tile, extendedSizeToBeSafe) &&
              secondLayer.freeAndInBounds(tile, extendedSizeToBeSafe)
            }.map { target =>
              Orders.MoveToTile(unit, target)
            }
          } else {
            None
          }
        }
        lastTarget = order.map(_.to)
        order.toList
      }
    }
    private def tolerance = 2
  }

  class PreventBlockades(universe: Universe) extends DefaultBehaviour[Mobile](universe) {

    private val newBlockingUnits   = oncePer(Primes.prime37) {
      val baseArea = mapLayers.freeWalkableTiles.guaranteeImmutability
      val unitsToPositions = ownUnits.allCompletedMobiles
                             .iterator
                             .filter(_.canMove)
                             .filter(_.surroundings.closeOwnBuildings.size > 3)
                             .map { e => e.nativeUnitId -> e.blockedArea }
                             .toMap

      val buildingLayer = mapLayers.freeWalkableTiles.guaranteeImmutability
      BWFuture.produceFrom {
        val badlyPositioned = unitsToPositions.flatMap { case (id, where) =>
          val withOutline = where.growBy(tolerance)

          import scala.collection.breakOut
          val touched: Set[Grid2D] = withOutline.tiles.flatMap { tile =>
            if (baseArea.inBounds(tile)) baseArea.areaWhichContainsAsFree(tile) else None
          }(breakOut)
          def nearBuilding = {
            buildingLayer.anyBlocked(where.growBy(5))
          }
          if (touched.size > 1 && nearBuilding) {
            Some(where.upperLeft -> id)
          } else {
            None
          }
        }
        mutable.HashMap.empty ++= badlyPositioned
      }
    }
    private val blockingUnitsQueue = mutable.HashSet.empty[Mobile]
    private val relevantLayer      = oncePerTick {
      mapLayers.freeWalkableTiles.guaranteeImmutability
    }
    override def forceRepeatedCommands: Boolean = false

    override def onTick() = {
      super.onTick()
      newBlockingUnits.get.ifDoneOpt { locking =>
        val problems = locking.flatMap { case (_, id) =>
          ownUnits.byId(id).asInstanceOf[Option[Mobile]]
        }.filter { unit =>
          universe.unitGrid.enemy.allInRange[Mobile](unit.currentTile, 5).isEmpty
        }
        blockingUnitsQueue ++= problems
        if (problems.nonEmpty) {
          info(s"${problems.size} new units identified as area splitting")
        }
        debug(s"${blockingUnitsQueue.size} units identified as area splitting right now")
        locking.clear()
      }
    }

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {

      override def describeShort: String = "<->"
      private var lastFreeGoTo = Option.empty[MapTilePosition]

      override def toOrder(what: Objective): Seq[UnitOrder] = {
        if (blockingUnitsQueue(unit)) {
          val layer = relevantLayer.get
          val myBuildings = mapLayers.blockedByBuildingTiles
          val safeArea = unit.blockedArea.growBy(tolerance)
          val command = lastFreeGoTo.filter { check =>
            layer.freeAndInBounds(safeArea)
          }.map(Orders.MoveToTile(unit, _)).orElse {
            val moveTo = layer.spiralAround(unit.currentTile, 45)
                         .sliding(1, 5)
                         .flatten
                         .filter(layer.freeAndInBounds)
                         .find { e =>
                           layer.freeAndInBounds(safeArea.growBy(1).moveTo(e))
                         }
            if (moveTo.isEmpty) {
              skipFor(Primes.prime59.i)
            }

            moveTo.map { whereTo =>
              lastFreeGoTo = Some(whereTo)
              Orders.MoveToTile(unit, whereTo)
            }
          }.toList
          if (command.isEmpty ||
              lastFreeGoTo.map(_.distanceTo(unit.currentTile)).getOrElse(0.0) < 1.5 ||
              !layer.cuttingAreas(safeArea)) {
            blockingUnitsQueue -= unit
          }
          command
        } else {
          Nil
        }
      }
    }
    private def tolerance = 2
  }

  class RepairDamagedUnit(universe: Universe) extends DefaultBehaviour[SCV](universe) {

    override def priority = SecondPriority.More

    private val helper = new NonConflictingTargets[Mechanic, SCV](universe = universe,
      rateTarget = m => PriorityChain(m.percentageHPOk),
      validTargetTest = _.isDamaged,
      subAccept = (m, t) => m.currentArea == t.currentArea,
      subRate = (m, t) => PriorityChain(-m.currentTile.distanceSquaredTo(t.currentTile)),
      own = true,
      allowReplacements = true)

    override def onTick() = {
      super.onTick()
      helper.onTick()
    }
    override protected def wrapBase(unit: SCV) = new SingleUnitBehaviour[SCV](unit, meta) {

      override def onStealUnit() = {
        super.onStealUnit()
        helper.unlock_!(unit)
      }
      override def describeShort: String = "Repair unit"
      override def toOrder(what: Objective): Seq[UnitOrder] = {
        helper.suggestTarget(unit).map { what =>
          Orders.RepairUnit(unit, what)
        }.toList
      }
    }

  }

  class RepairDamagedBuilding(universe: Universe) extends DefaultBehaviour[SCV](universe) {

    override def priority = SecondPriority.More

    private val helper = new NonConflictingTargets[TerranBuilding, SCV](universe = universe,
      rateTarget = m => PriorityChain(m.percentageHPOk),
      validTargetTest = _.isDamaged,
      subAccept = (m, t) => m.currentArea == t.areaOnMap,
      subRate = (m, t) => PriorityChain(-m.currentTile.distanceSquaredTo(t.centerTile)),
      own = true,
      allowReplacements = true)

    override def onTick() = {
      super.onTick()
      helper.onTick()
    }

    override protected def wrapBase(unit: SCV) = new SingleUnitBehaviour[SCV](unit, meta) {
      override def onStealUnit() = {
        super.onStealUnit()
        helper.unlock_!(unit)
      }

      override def describeShort: String = "Repair building"
      override def toOrder(what: Objective): Seq[UnitOrder] = {
        helper.suggestTarget(unit).map { what =>
          Orders.RepairBuilding(unit, what)
        }.toList
      }
    }
  }

  class ContinueInterruptedConstruction(universe: Universe)
    extends DefaultBehaviour[SCV](universe) {

    override def priority = SecondPriority.More

    private val area = oncePerTick {
      mapLayers.rawWalkableMap.guaranteeImmutability
    }

    private val helper = new NonConflictingTargets[Building, SCV](
      universe = universe,
      rateTarget = b => PriorityChain(-b.remainingBuildTime),
      validTargetTest = e => e.isIncompleteAbandoned && !e.isInstanceOf[Addon],
      subRate = (w, b) => PriorityChain(-w.currentTile.distanceSquaredTo(b.tilePosition)),
      own = true,
      allowReplacements = true,
      subAccept = (w, b) => true)

    override def onTick() = {
      super.onTick()
      helper.onTick()
    }

    override protected def wrapBase(unit: SCV) = new SingleUnitBehaviour[SCV](unit, meta) {

      private var target = Option.empty[Building]

      override def canInterrupt = {
        super.canInterrupt && !unit.isInConstructionProcess &&
        (target.isEmpty || !target.exists(_.incomplete))
      }

      override def describeShort: String = "Finish construction"

      override def toOrder(what: Objective): Seq[UnitOrder] = {
        def eval = helper.suggestTarget(unit)
        target.filter(_.incomplete).orElse(eval).map { building =>
          target = Some(building)
          val sameArea = area.get.areInSameWalkableArea(unit.currentTile, building.centerTile)
          if (sameArea) {
            Orders.ContinueConstruction(unit, building)
          } else {
            Orders.MoveToTile(unit, building.centerTile)
          }
        }.toList
      }
    }
  }

  class MigrateTowardsPosition(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    override def priority = SecondPriority.Less

    override def canControl(u: WrapsUnit): Boolean = super.canControl(u) &&
                                                     !u.isInstanceOf[WorkerUnit] &&
                                                     !u.isInstanceOf[TransporterUnit] &&
                                                     !u.isInstanceOf[AutoPilot]

    override def onTick(): Unit = {
      super.onTick()
    }

    override def renderDebug(renderer: Renderer) = {
      super.renderDebug(renderer)
      worldDominationPlan.allAttacks.foreach { plan =>
        plan.migrationPlan.foreach { p =>
          p.targetFormationTiles.foreach { tile =>
            renderer.in_!(Color.Cyan).drawCircleAround(tile)
          }
          renderer.in_!(Color.Red).drawStar(p.finalDestination)
          renderer.in_!(Color.Green).drawStar(p.safeDestination)
        }
      }
    }

    private class DefaultMigrationBehaviour[+T <: Mobile](unit: T)
      extends SingleUnitBehaviour(unit, meta) {
      override def describeShort = "Migrate"
      override protected def toOrder(what: Objective) = {
        worldDominationPlan.attackOf(unit).map { attack =>
          attack.suggestActionFor(unit).asOrder.toList
        }.getOrElse(Nil)
      }
    }

    override protected def wrapBase(unit: Mobile): SingleUnitBehaviour[Mobile] = {
      def finalDestination = {
        worldDominationPlan.attackOf(unit).map { attack =>
          attack.destination.where
        }
      }
      val behaviour = unit match {
        case g: GroundUnit =>
          new DefaultMigrationBehaviour[GroundUnit](g) with FerrySupport[GroundUnit] {
            override protected def suggestFerryDropPosition = {
              finalDestination
            }
          }

        case m => new DefaultMigrationBehaviour[Mobile](unit)
      }
      behaviour
    }
  }

  class ReallyReallyLazy(universe: Universe) extends DefaultBehaviour[Mobile](universe) {

    override def priority = SecondPriority.None

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {
      override def describeShort = "Bored"
      override def toOrder(what: Objective) = {
        Orders.NoUpdate(unit).toList
      }
    }
  }

  class StimSelf(universe: Universe) extends DefaultBehaviour[CanUseStimpack](universe) {
    override protected def wrapBase(unit: CanUseStimpack) = new
        SingleUnitBehaviour[CanUseStimpack](unit, meta) {

      override def preconditionOk = upgrades.hasResearched(InfantryCooldown)

      override def toOrder(what: Objective) = {
        if (unit.isAttacking && !unit.isStimmed) {
          List(Orders.TechOnSelf(unit, InfantryCooldown))
        } else {
          Nil
        }
      }
      override def describeShort: String = s"Stimpack"
    }
  }

  class SiegeUnsiegeSelf(universe: Universe) extends DefaultBehaviour[Tank](universe) {

    override def forceRepeatedCommands = true

    override protected def wrapBase(unit: Tank) = new SingleUnitBehaviour[Tank](unit, meta) {

      override def preconditionOk = upgrades.hasResearched(TankSiegeMode)

      override def toOrder(what: Objective) = {
        val enemies = unit.surroundings.closeEnemyGroundUnits
        val buildings = unit.surroundings.closeEnemyBuildings

        def buildingInRange = buildings.exists(_.area.distanceTo(unit.currentTile) <= 11)

        def siegeableInRange = {
          buildingInRange || enemies.iterator.filterNot(_.isHarmlessNow).take(4).size >= 3
        }
        def anyNear = {
          enemies.exists(e => !e.isHarmlessNow && e.centerTile.distanceToIsMore(unit.centerTile, 4))
        }

        if (unit.isSieged) {
          if (buildingInRange || anyNear) {
            Nil
          } else {
            Orders.TechOnSelf(unit, TankSiegeMode).toList
          }
        } else {
          if (siegeableInRange) {
            Orders.TechOnSelf(unit, TankSiegeMode).toList
          } else {
            Nil
          }
        }
      }
      override def describeShort: String = s"Siegemode"
    }
  }

  class CloakSelfWraith(universe: Universe) extends DefaultBehaviour[Wraith](universe) {
    override protected def wrapBase(unit: Wraith) = new SingleUnitBehaviour[Wraith](unit, meta) {

      override def preconditionOk = upgrades.hasResearched(WraithCloak)

      override def toOrder(what: Objective) = {
        if (unit.isBeingAttacked && !unit.isCloaked) {
          List(Orders.TechOnSelf(unit, WraithCloak))
        } else {
          Nil
        }
      }
      override def describeShort: String = s"Cloak"
    }
  }

  class CloakSelfGhost(universe: Universe) extends DefaultBehaviour[Ghost](universe) {
    override protected def wrapBase(unit: Ghost) = new SingleUnitBehaviour[Ghost](unit, meta) {

      override def preconditionOk = upgrades.hasResearched(GhostCloak)

      override def toOrder(what: Objective) = {
        if (unit.isBeingAttacked && !unit.isCloaked) {
          List(Orders.TechOnSelf(unit, GhostCloak))
        } else {
          Nil
        }
      }
      override def describeShort: String = s"Cloak"
    }
  }

  class GoToInitialPosition(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    private val helper = new FormationAtFrontLineHelper(universe)

    private val ignore = mutable.HashSet.empty[Mobile]

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit,
      meta) {
      override def describeShort: String = "Goto IP"
      override def toOrder(what: Objective) = {
        if (universe.time.minutes <= 5 || ignore(unit) || unit.isBeingCreated) {
          Nil
        } else {
          helper.allInsideNonBlacklisted.toStream.headOption.map { where =>
            ignore += unit
            helper.blacklisted(where)
            Orders.AttackMove(unit, where)
          }.toList
        }
      }
    }
  }

  class UseComsat(universe: Universe) extends DefaultBehaviour[Comsat](universe) {
    private val detectThese = ArrayBuffer.empty[Group[CanCloak]]

    override def onTick() = {
      super.onTick()
      detectThese ++= {
        mapNth(Primes.prime31, Seq.empty[Group[CanCloak]]) {
          val dangerous = {
                            enemies.allByType[CanCloak]
                            .iterator
                            .filterNot(_.isDecloaked)
                            .filter { cloaked =>
                              ownUnits.allCompletedMobiles
                              .exists(_.centerTile.distanceToIsLess(cloaked.centerTile, 8))
                            }
                          }.toVector

          val groups = GroupingHelper.groupTheseNow(dangerous, universe)
          groups.sortBy(-_.size)
        }
      }

      debug(s"Currently known cloaked groups: ${
        detectThese.map(e => s"${e.size}@${e.center}").mkString(", ")
      })", detectThese.nonEmpty)
    }

    override protected def wrapBase(comsat: Comsat) = new
        SingleUnitBehaviour[Comsat](comsat, meta) {
      override def describeShort = "Scan"
      override def toOrder(what: Objective) = {
        if (detectThese.nonEmpty && comsat.canCastNow(ScannerSweep)) {
          val first = detectThese.remove(0)
          Orders.ScanWithComsat(comsat, first.center).toList
        } else {
          Nil
        }
      }
    }
  }

  class SetupMineField(universe: Universe) extends DefaultBehaviour[Vulture](universe) {
    val beLazy = Idle -> Nil
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
    override def renderDebug(renderer: Renderer): Unit = {
      suggestMinePositions.foreach { tile =>
        renderer.in_!(Color.White).drawCircleAroundTile(tile)
      }
    }
    override def onTick(): Unit = {
      super.onTick()
      ifNth(Primes.prime137) {
        helper.cleanBlacklist((_, reason) => reason.when + 240 < universe.currentTick)
      }
    }
    override def priority = SecondPriority.EvenLess
    override protected def wrapBase(unit: Vulture) = new SingleUnitBehaviour[Vulture](
      unit, meta) {

      private var state: State            = Idle
      private var originalSpiderMineCount = unit.spiderMineCount

      def freeArea = mined.get
      private var inBattle = false

      override def priority = if (inBattle) SecondPriority.EvenMore else super.priority
      override def describeShort: String = "Minefield"
      override def toOrder(what: Objective) = {
        val (newState, orders) = state match {
          case Idle =>
            // TODO include test in tech trait
            if (unit.spiderMineCount > 0 && unit.canCastNow(SpiderMines)) {
              val enemies = universe.unitGrid.enemy.allInRange[GroundUnit](unit.currentTile, 5)
              if (enemies.nonEmpty) {
                inBattle = true
                // drop mines on sight of enemy
                val on = freeArea
                val freeTarget = on.spiralAround(unit.currentTile).filter(on.free)
                                 .maxByOpt { where =>
                                   def ownUnitsCost = {
                                     universe.unitGrid.own.allInRange[GroundUnit](where, 5)
                                     .view
                                     .filter(
                                       e => !e.isInstanceOf[HasSpiderMines] && !e.isAutoPilot)
                                     .map(_.buildPrice)
                                     .fold(Price.zero)(_ + _)
                                   }
                                   def enemyUnitsCost = {
                                     universe.unitGrid.enemy.allInRange[GroundUnit](where, 5)
                                     .view
                                     .filter(
                                       e => !e.isInstanceOf[HasSpiderMines] && !e.isAutoPilot)
                                     .map(_.buildPrice)
                                     .fold(Price.zero)(_ + _)
                                   }
                                   enemyUnitsCost - ownUnitsCost
                                 }
                freeTarget.map { where =>
                  on.block_!(where.asArea.extendedBy(1))
                  plannedDrops += where.asArea.extendedBy(1) -> universe.currentTick
                  DroppingMine(where) -> unit.toOrder(SpiderMines, where).toList
                }.getOrElse(beLazy)
              } else {
                inBattle = false
                // place mines on strategic positions
                val candiates = suggestMinePositions
                val dropMineHere = candiates.minByOpt(_.distanceSquaredTo(unit.currentTile))
                dropMineHere.foreach(helper.blackList_!)
                dropMineHere.map { where =>
                  DroppingMine(where) -> unit.toOrder(SpiderMines, where).toList
                }.getOrElse(beLazy)
              }
            } else {
              inBattle = false
              beLazy
            }

          case myState@DroppingMine(where) if unit.canCastNow(SpiderMines) =>
            myState -> unit.toOrder(SpiderMines, where).toList
          case DroppingMine(_) if unit.spiderMineCount < originalSpiderMineCount =>
            inBattle = false
            originalSpiderMineCount = unit.spiderMineCount
            Idle -> Nil
          case myState@DroppingMine(_) =>
            inBattle = false
            myState -> Nil
        }
        state = newState
        orders
      }
      override def preconditionOk: Boolean = universe.upgrades
                                             .hasResearched(Upgrades.Terran.SpiderMines)
    }
    private def suggestMinePositions = {
      val defense = helper.allOutsideNonBlacklisted
      val neutralResourceFields = universe.resourceFields.resourceAreas.filterNot { field =>
        universe.bases.isCovered(field)
      }.map {_.mostAnnoyingMinePosition}
      defense //++ neutralResourceFields
    }
    trait State
    case class DroppingMine(tile: MapTilePosition) extends State

    case object Idle extends State
  }

  class Scout(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    override protected def wrapBase(t: Mobile) = ???
  }

  class DoNotStray(universe: Universe) extends DefaultBehaviour[SupportUnit](universe) {
    override protected def wrapBase(t: SupportUnit) = ???
  }

  class OneTimeUnitSpellCast[C <: HasSingleTargetSpells : Manifest, T <: Mobile : Manifest]
  (universe: Universe,
   spell:
   SingleTargetSpell[C, T])
    extends DefaultBehaviour[C](universe) {
    private val helper = NonConflictingSpellTargets.forSpell(spell, universe)

    universe.register_!(() => {
      helper.afterTick()
    })

    override def onTick() = {
      super.onTick()
    }
    override def refuseCommandsForTicks = 12

    override def priority: SecondPriority = SecondPriority.Max

    override protected def wrapBase(unit: C): SingleUnitBehaviour[C] = new
        SingleUnitBehaviour[C](unit, meta) {
      override def describeShort: String = s"Cast ${spell.getClass.className}"
      override def toOrder(what: Objective): Seq[UnitOrder] = {
        if (unit.canCastNow(spell.tech)) {
          val h = helper
          h.suggestTargetFor(unit).map { target =>
            h.notifyLock_!(unit, target)
            unit.toOrder(spell.tech, target)
          }.toList
        } else {
          Nil
        }
      }
    }
  }

  class StopMechanic(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Lockdown)
  class ShieldUnit(universe: Universe)
    extends OneTimeUnitSpellCast(universe, Spells.DefenseMatrix)
  class IrradiateUnit(universe: Universe)
    extends OneTimeUnitSpellCast(universe, Spells.Irradiate)

  class HealDamagedUnit(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def wrapBase(t: Medic) = ???
  }
  class FixMedicalProblem(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def wrapBase(t: Medic) = ???
  }
  class BlindDetector(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def wrapBase(t: Medic) = ???
  }
  class FocusFire(universe: Universe) extends DefaultBehaviour[MobileRangeWeapon](universe) {

    private val helper = new FocusFireOrganizer(universe)

    override def onTick() = {
      super.onTick()
      helper.onTick()
    }

    override protected def wrapBase(unit: MobileRangeWeapon) = new
        SingleUnitBehaviour(unit, meta) {
      override def describeShort = "Focus fire"
      override def toOrder(what: Objective) = {
        helper.suggestTarget(unit).map { target =>
          Orders.AttackUnit(unit, target)
        }.toList
      }
    }
  }
}

case class Target[T <: Mobile](caster: HasSingleTargetSpells, target: T)

class FocusFireOrganizer(override val universe: Universe) extends HasUniverse {

  override def onTick() = {
    super.onTick()
    enemy2Attackers.filter(_._1.isDead).foreach { case (dead, attackers) =>
      trace(s"Unit $dead died, reorganizing attackers")
      enemy2Attackers.remove(dead)
      attackers.allAttackers.foreach { unit =>
        me2Enemy.remove(unit)
      }
    }

    me2Enemy.keySet.filter(_.isDead).foreach { dead =>
      val target = me2Enemy.remove(dead)
      target.foreach { canDie =>
        enemy2Attackers(canDie).removeAttacker_!(dead)
      }
    }

    invalidateQueue()
  }

  private val enemy2Attackers    = mutable.HashMap.empty[MaybeCanDie, Attackers]
  private val me2Enemy           = mutable.HashMap.empty[MobileRangeWeapon, MaybeCanDie]
  private val prioritizedTargets = LazyVal.from {
    val prioritized = universe.enemyUnits.allCanDie.toVector.sortBy { e =>
      (e.isHarmlessNow.ifElse(1, 0), enemy2Attackers.contains(e).ifElse(0, 1), -e.price.sum, e
                                                                                             .hitPoints
                                                                                             .sum)
    }
    prioritized
  }
  def suggestTarget(myUnit: MobileRangeWeapon): Option[MaybeCanDie] = {
    val maybeAttackers = me2Enemy.get(myUnit)
                         .map(enemy2Attackers)
    val shouldLeaveTeam = {
      def outOfRange = maybeAttackers.exists(_.isOutOfRange(myUnit))
      def overkill = maybeAttackers
                     .filter(_.isOverkill)
                     .exists(_.canSpare(myUnit))
      outOfRange || overkill
    }
    if (shouldLeaveTeam) {
      maybeAttackers.foreach(_.removeAttacker_!(myUnit))
      me2Enemy.remove(myUnit)
    }

    prioritizedTargets.get.find { target =>
      val existing = enemy2Attackers.get(target).exists(_.isAttacker(myUnit))
      existing || (myUnit.isInWeaponRange(target) && myUnit.canAttack(target) &&
                   enemy2Attackers.getOrElseUpdate(target, new Attackers(target)).canTakeMore)
    }.foreach { attackThis =>
      val plan = enemy2Attackers(attackThis)
      if (!plan.isAttacker(myUnit)) {
        me2Enemy.put(myUnit, attackThis)
        plan.addAttacker_!(myUnit)
      }
      invalidateQueue()
    }
    me2Enemy.get(myUnit)
  }

  def invalidateQueue(): Unit = {
    prioritizedTargets.invalidate()
  }

  class Attackers(val target: MaybeCanDie) {
    class MutableHP(var hitPoints: Int, var shieldPoints: Int) extends HasHpAndShields {
      override val hp      = hitPoints
      override val shields = shieldPoints

      def toHP = HitPoints(hitPoints, shieldPoints)

      def set(hp: MutableHP): Unit = {
        this.hitPoints = hp.hitPoints
        shieldPoints = hp.shieldPoints
      }

      def +!(damageDone: HasHpAndShields) = {
        hitPoints += damageDone.hp
        shieldPoints += damageDone.shields
        this
      }

      assert(shieldPoints >= 0)

      def alive = hitPoints > 0

      def -!(dsa: HasHpAndShields) = {
        hitPoints -= dsa.hp
        shieldPoints -= dsa.shields
        this
      }

    }

    private val attackers     = mutable.HashSet.empty[MobileRangeWeapon]
    private val plannedDamage = mutable.HashMap
                                .empty[MobileRangeWeapon, DamageSingleAttack]
    private def currentHp = new MutableHP(actualHP.hitpoints, actualHP.shield)

    private val hpAfterNextAttacks  = currentHp
    private val plannedDamageMerged = new MutableHP(0, 0)

    def isOutOfRange(myUnit: MobileRangeWeapon) = !myUnit.isInWeaponRange(target)

    def removeAttacker_!(t: MobileRangeWeapon): Unit = {
      attackers -= t
      plannedDamage -= t
      recalculatePlannedDamage_!()
      hpAfterNextAttacks.set(currentHp -! plannedDamageMerged)
      invalidateQueue()
    }

    def canSpare(attacker: MobileRangeWeapon) = {
      assert(isAttacker(attacker))
      // this is not entirely correct because of shields & zerg regeneration, but should be
      // close enough
      val damage = plannedDamage(attacker)

      val predicted = (plannedDamageMerged.hitPoints - damage.onHp,
        plannedDamageMerged.shieldPoints - damage.onShields)
      actualHP < predicted
    }

    def isAttacker(t: MobileRangeWeapon) = attackers(t)
    def allAttackers = attackers.iterator
    def addAttacker_!(t: MobileRangeWeapon): Unit = {
      assert(!attackers(t), s"$attackers already contains $t")
      attackers += t
      // for slow attacks, we assume that one is always on the way to hit to avoid overkill
      val expectedDamage = {
        val factor = 1 + t.assumeShotDelayOn(target)

        t.calculateDamageOn(target, hpAfterNextAttacks.hitPoints, hpAfterNextAttacks.shieldPoints,
          factor)
      }

      plannedDamage.put(t, expectedDamage)
      recalculatePlannedDamage_!()
      hpAfterNextAttacks -! expectedDamage
    }

    def recalculatePlannedDamage_!(): Unit = {
      val attackersSorted = plannedDamage.keys.toArray.sortBy(_.cooldownTimer)
      val damage = new MutableHP(0, 0)
      val currentHp = actualHP
      val assumeHp = new MutableHP(currentHp.hitpoints, currentHp.shield)
      attackersSorted.foreach { attacker =>
        val shotCount = 1 + attacker.assumeShotDelayOn(target)
        val moreDamage = attacker
                         .calculateDamageOn(target, assumeHp.hitPoints, assumeHp.shieldPoints,
                           shotCount)
        damage +! moreDamage
        assumeHp -! moreDamage
      }
      plannedDamageMerged.set(damage)
    }

    private def actualHP = target.hitPoints
    def isOverkill = actualHP <(plannedDamageMerged.hitPoints, plannedDamageMerged.shieldPoints)
    def canTakeMore = hpAfterNextAttacks.alive

  }
}

object NonConflictingSpellTargets {
  def forSpell[T <: HasSingleTargetSpells, M <: Mobile : Manifest](spell:
                                                                   SingleTargetSpell[T, M],
                                                                   universe: Universe) = {

    new NonConflictingSpellTargets(spell, {
      case x: M if spell.canBeCastOn(x) & spell.shouldActivateOn(x) => x
    },
      spell.isAffected, universe)
  }
}

class NonConflictingTargets[T <: WrapsUnit : Manifest, M <: Mobile : Manifest]
(override val universe: Universe, rateTarget: T => PriorityChain, validTargetTest: T => Boolean,
 subAccept: (M, T) => Boolean, subRate: (M, T) => PriorityChain, own: Boolean,
 allowReplacements: Boolean) extends HasUniverse {

  private val validTarget = (t: T) => t.isInGame && validTargetTest(t)

  override def onTick() = {
    super.onTick()
    val noLongerValid = assignments.filter { case (m, t) =>
      !m.isInGame || !validTarget(t)
    }
    noLongerValid.foreach { case (k, v) => unlock_!(k, v) }
  }

  private val locks              = mutable.HashSet.empty[T]
  private val assignments        = mutable.HashMap.empty[M, T]
  private val assignmentsReverse = mutable.HashMap.empty[T, M]
  private val targets            = universe.oncePerTick {
    val on = if (own) ownUnits else enemies
    on.allByType[T].iterator
    .filter(validTarget)
    .map { e => e -> rateTarget(e) }
    .toVector
    .sortBy(_._2)
    .map(_._1)
  }
  def suggestTarget(m: M) = {
    assignments.get(m) match {
      case x@Some(target) =>
        if (validTarget(target))
          x
        else {
          unlock_!(m, target)
          None
        }
      case None =>
        val newSuggestion = targets.get
                            .iterator
                            .filterNot(locked)
                            .filter(subAccept(m, _))
                            .maxByOpt(subRate(m, _))
        newSuggestion match {
          case Some(t) =>
            lock_!(t, m)
            newSuggestion
          case None =>
            if (allowReplacements) {
              val bestToReplace =
                locks
                .iterator
                .filter(subAccept(m, _))
                .maxByOpt(subRate(m, _))

              bestToReplace match {
                case Some(maybeStealMe) =>
                  val lockedOn = assignmentsReverse(maybeStealMe)
                  val ord: Ordering[PriorityChain] = implicitly
                  if (ord.lt(subRate(lockedOn, maybeStealMe), subRate(m, maybeStealMe))) {
                    unlock_!(lockedOn, maybeStealMe)
                    lock_!(maybeStealMe, m)
                    Some(maybeStealMe)
                  } else {
                    None
                  }
                case None => None
              }
            } else {
              None
            }

        }
    }
  }
  def unlock_!(m: M, target: T): Unit = {
    assignments.remove(m)
    assignmentsReverse.remove(target)
    locks -= target
  }
  def unlock_!(m: M): Unit = {
    if (assignments.contains(m)) {
      val target = assignments(m)
      unlock_!(m, target)
    }
  }

  def lock_!(t: T, m: M): Unit = {
    assert(!locked(t))
    assert(!assignments.contains(m))

    assignments.put(m, t)
    assignmentsReverse.put(t, m)
    locks += t
  }
  private def locked(t: T) = locks(t)
  private def targetOf(m: M) = assignments(m)
}

class NonConflictingSpellTargets[T <: HasSingleTargetSpells, M <: Mobile : Manifest](spell:
                                                                                     SingleTargetSpell[T, M],
                                                                                     targetConstraint:
                                                                                     PartialFunction[Mobile, M],
                                                                                     keepLocked:
                                                                                     M => Boolean,
                                                                                     override val
                                                                                     universe:
                                                                                     Universe)
  extends HasUniverse {
  private val locked = mutable.HashSet.empty[M]

  private val lockedTargets      = mutable.HashSet.empty[Target[M]]
  private val assignments        = mutable.HashMap.empty[M, Target[M]]
  private val prioritizedTargets = LazyVal.from {
    val base = {
      val targets = {
        if (spell.castOn == EnemyUnits) {
          universe.enemyUnits.allByType[M]
        } else {
          universe.ownUnits.allByType[M]
        }
      }
      targets.collect(targetConstraint)
    }

    spell.priorityRule.fold(base.toVector) { rule =>
      base.toVector.sortBy(m => -rule(m))
    }
  }
  def afterTick(): Unit = {
    prioritizedTargets.invalidate()
    locked.filterNot(keepLocked).foreach { elem =>
      unlock_!(elem)
    }
  }
  private def unlock_!(target: M): Unit = {
    locked -= target
    val old = assignments.remove(target).get
    lockedTargets -= old
    prioritizedTargets.invalidate()
  }
  def notifyLock_!(t: T, target: M): Unit = {
    locked += target
    val tar = Target(t, target)
    lockedTargets += tar
    assignments.put(target, tar)
    prioritizedTargets.invalidate()
  }
  def suggestTargetFor(caster: T): Option[M] = {
    // for now, just pick the first in range that is not yet taken
    val range = spell.castRangeSquare

    val filtered = prioritizedTargets.get.filterNot(locked)
    filtered.find {_.currentPosition.distanceSquaredTo(caster.currentPosition) < range}
  }
}



