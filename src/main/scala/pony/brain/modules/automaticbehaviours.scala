package pony
package brain
package modules

import bwapi.Color
import pony.Upgrades.Terran._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class DefaultBehaviour[T <: WrapsUnit : Manifest](override val universe: Universe)
  extends HasUniverse {

  private val asEmployer      = new Employer[T](universe)
  private val unit2behaviour  = mutable.HashMap.empty[T, SingleUnitBehaviour[T]]
  private val controlledUnits = mutable.HashSet.empty[T]

  def employer = asEmployer

  override def onTick_!(): Unit = {
    super.onTick_!()
    val remove = controlledUnits.filterNot(_.isInGame)
    controlledUnits --= remove
    unit2behaviour --= remove
  }

  def renderDebug_!(renderer: Renderer): Unit = {}

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
    val allOfThem = (new StimSelf(universe) ::
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
                     new MoveAwayFromDangerousSpotOnGround(universe) ::
                     new MoveAwayFromDangerousSpotOnAir(universe) ::
                     new PreventBlockades(universe) ::
                     new ContinueInterruptedConstruction(universe) ::
                     new UseComsat(universe) ::
                     new Scout(universe) ::
                     /*
                                          new DoNotStray ::
                                          new HealDamagedUnit ::
                                          new FixMedicalProblem ::
                                          new BlindDetector ::
                     */
                     new Dance(universe) ::
                     new HelpNearUnits(universe) ::
                     new DeliverResources(universe) ::
                     new FocusFire(universe) ::
                     new ReallyReallyLazy(universe) ::
                     Nil)
                    .map(_.cast)
    allOfThem
  }

  class TransportGroundUnits(universe: Universe)
    extends DefaultBehaviour[TransporterUnit](universe) {

    private val timeBetweenUpdates = 24
    private val maxAge             = 24 * 4

    override def forceRepeatedCommands: Boolean = false

    override protected def wrapBase(unit: TransporterUnit) = new
        SingleUnitBehaviour[TransporterUnit](unit, meta) {

      override def renderDebug(r: Renderer) = {
        super.renderDebug(r)
        ferryManager.planFor(unit).foreach { plan =>
          val describe = {
            val fly = if (plan.needsToReachTarget) "fly, " else ""
            val unload = if (plan.dropUnitsNow) "unload, " else ""
            val fetch = if (plan.pickupTargetsLeft) "fetch, " else ""
            val instantDrop = if (plan.instantDropRequested) "drop, " else ""
            s"$fly$unload$fetch$instantDrop"
          }

          if (plan.needsToReachTarget) {
            r.in_!(Color.Green)
            r.indicateTarget(unit.currentTile, plan.toWhere)
          }
          if (plan.pickupTargetsLeft) {
            r.in_!(Color.Orange)
            r.indicateTarget(unit.currentTile, plan.nextToPickUp.map(_.currentTile).get)
          }

          if (plan.dropUnitsNow) {
            r.in_!(Color.Red)
            r.indicateTarget(unit.currentTile, plan.nextToDrop.map(_.currentTile).get)
          }

          r.drawTextAtMobileUnit(unit, describe, 2)
        }
      }

      trait PositionOrUnit {
        def where: MapTilePosition
        def unit: Option[GroundUnit]
      }

      case class Feed(transporterWhere: MapTilePosition, pickupTarget: PositionOrUnit,
                      pathfinder: PathFinder)

      case class IsPosition(where: MapTilePosition) extends PositionOrUnit {
        override def unit = None
      }

      case class IsUnit(basedOn: GroundUnit) extends PositionOrUnit {
        override val where = basedOn.currentTile

        private val u = basedOn.toSome

        override def unit = u
      }

      case class MaybePath(path: FutureIterator[Feed, Option[MigrationPath]]) {

        def onTick_!() = {
          path.onMostRecent(_.foreach(_.onTick_!()))
        }

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

      override def onTick_!() = {
        super.onTick_!()
        paths.valuesIterator.foreach(_.onTick_!())
      }

      override def toOrder(what: Objective): Seq[UnitOrder] = {
        val old = {
          paths.filter {
            case (_, maybe) => maybe.age > maxAge
          }
          .keySet
          .toList
        }
        paths --= old
        trace(s"Kicked out obsolete paths for: $old", old.nonEmpty)

        def currentSafeOrder(transporterTarget: PositionOrUnit): Option[UnitOrder] = {
          val maybeCalculatedPath = paths.getOrElseUpdate(transporterTarget, {
            def feed = Feed(unit.currentTile, transporterTarget, pathfinders.airSafe)

            val future = FutureIterator.feed(feed).produceAsync { in =>
              in.pathfinder.findPathNow(in.transporterWhere, in.pickupTarget.where)
              .map(_.toMigration)
            }.named("Pathfinding")
            MaybePath(future)
          })
          if (maybeCalculatedPath.age > timeBetweenUpdates) {
            maybeCalculatedPath.updateFuture()
          }
          maybeCalculatedPath.path.flatMapOnContent { maybeFoundPath =>
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
                safePath.nextPositionFor(unit).map { where =>
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

        val order = {
          ferryManager.planFor(unit) match {
            case Some(plan) =>
              if ((plan.instantDropRequested || plan.dropUnitsNow) && !unit.canDropHere) {
                unit.nearestDropTile.flatMap(orderByTile)
              } else if (plan.instantDropRequested && unit.canDropHere) {
                plan.asapDrop.map { dropIt =>
                  Orders.UnloadUnit(unit, dropIt)
                }
              } else if (plan.dropUnitsNow) {
                plan.nextToDrop.map { drop =>
                  Orders.UnloadUnit(unit, drop)
                }
              } else if (plan.pickupTargetsLeft) {
                val loadThis = plan.nextToPickUp
                orderByUnit(loadThis.get)
              } else if (plan.needsToReachTarget) {
                orderByTile(plan.toWhere)
              } else {
                None
              }
            case None =>
              if (unit.hasUnitsLoaded) {
                val nearestFree = mapLayers.freeWalkableTiles.nearestFree(unit.currentTile)
                nearestFree.map { where =>
                  Orders.UnloadAll(unit, where).forceRepeat_!(true)
                }
              } else if (unit.isPickingUp) {
                Orders.Stop(unit).toSome
              }
              else {
                None
              }
          }
        }
        order.toList
      }
    }
  }

  class DeliverResources(universe: Universe) extends DefaultBehaviour[WorkerUnit](universe) {

    override def priority = SecondPriority.BetterThanNothing

    override protected def wrapBase(unit: WorkerUnit) = new SingleUnitBehaviour(unit, meta) {
      override def describeShort = "Return resources"

      override protected def toOrder(what: Objective) = {
        if (unit.isCarryingGas || unit.isCarryingMinerals) {
          Orders.ReturnResourcesToAnyBase(unit).toList
        } else {
          Nil
        }
      }
    }
  }

  class HelpNearUnits(universe: Universe) extends DefaultBehaviour[ArmedMobile](universe) {

    override def priority = SecondPriority.Less

    override protected def wrapBase(unit: ArmedMobile) = {
      new SingleUnitBehaviour[ArmedMobile](unit, meta) {
        override def describeShort = "(+)"

        override protected def toOrder(what: Objective) = {
          val closest = {
            unit.surroundings.closeOwnUnits
            .iterator
            .filter(_.isInFight)
            .filter { e =>
              mapLayers.rawWalkableMap.connectedByLine(e.currentTile, unit.currentTile)
            }
            .minByOpt(_.currentTile.distanceSquaredTo(unit.currentTile))
          }
          closest.map { helpThisOne =>
            Orders.AttackMove(unit, helpThisOne.currentTile)
          }.toList
        }
      }
    }
  }

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

    private val freeWalkableMap   = oncePerTick {
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
      val on = in.freeMap
      val safetyCheck = in.safeFromLongRange
      in.dancers.map { dancer =>
        val center = MapTilePosition.average(dancer.partners.iterator.map(_.where))

        val myArea = universe.mapLayers.rawWalkableMap.areaOf(dancer.me.where)
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
      }.collect { case (k, v) if v.isDefined =>
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

    override protected def wrapBase(unit: ArmedMobile): SingleUnitBehaviour[ArmedMobile] = new
        SingleUnitBehaviour[ArmedMobile](unit, meta) {

      trait State

      case object Idle extends State

      case class Fallback(to: MapTilePosition, startedAtTick: Int) extends State

      private var state: State    = Idle
      private var runningCommands = List.empty[UnitOrder]
      private val noop            = (Idle, List.empty[UnitOrder])

      override def describeShort: String = "Dance"

      override def toOrder(what: Objective) = {
        val (newState, newOrder) = {
          val canDance = !unit.isInstanceOf[BadDancer] || unit.hasBeenAttackedSince(8)
          if (unit.isReadyToFireWeapon || !canDance) {
            noop
          } else {
            state match {
              case Idle =>
                dancePlan.flatMapOnContent { plan =>
                  plan.get(unit.nativeUnitId)
                }.map { where =>
                  Fallback(where, universe.currentTick) -> Orders.MoveToTile(unit, where).toList
                }.getOrElse(noop)
              case current@Fallback(where, startedWhen) =>
                if (unit.isReadyToFireWeapon || unit.currentTile == where) {
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
            unitGrid.allInRangeOf[ArmedMobile](own.currentTile,
              own.weaponRangeRadiusTiles, friendly = false, take).map { unit =>
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

  abstract class AvoidSpecificAreas[T <: Mobile : Manifest](universe: Universe)
    extends DefaultBehaviour[T](universe) {
    self =>

    protected def tilesToAvoidAsBlocked: Option[Grid2D]

    protected def tolerance = 2

    case class Feed(toAvoid: Grid2D, free: Grid2D)

    protected def targetReuseAllowance = 0

    private def feed = Feed(tilesToAvoidAsBlocked.getOrElse(mapLayers.emptyGrid),
      mapLayers.freeWalkableTiles)

    private val freeAlternativeTiles = FutureIterator.feed(feed).produceAsyncLater { in =>
      val usages = mutable.HashMap.empty[MapTilePosition, Int]
      val walkable = mapLayers.rawWalkableMap
      val allBlocked = in.toAvoid.allBlocked.toList
      val mut = in.toAvoid.mutableCopy
      allBlocked.foreach { where =>
        mut.block_!(where.asArea.growBy(tolerance))
      }

      mut.allBlocked.toVector.flatMap { blocked =>
        val ret = in.free.spiralAround(blocked).find { tile =>
          in.free.freeAndInBounds(tile) &&
          mut.freeAndInBounds(tile) &&
          walkable.areInSameWalkableArea(tile, blocked) &&
          walkable.countBlockedOnLine(tile, blocked).freePercentage >= 0.8
        }
        // block solution for next try
        ret.foreach { found =>
          usages.insertReplace(found, _ + 1, 1)
          if (usages(found) == targetReuseAllowance) {
            mut.block_!(found)
          }
        }
        ret.map(e => blocked -> e)
      }.toMap
    }.named("Find alternative tiles")

    protected def debugColor = Color.Green

    override def renderDebug_!(renderer: Renderer) = {
      super.renderDebug_!(renderer)
      freeAlternativeTiles.foreach { data =>
        data.foreach { case (from, to) =>
          renderer.in_!(debugColor).drawCircleAround(to)
        }
      }
    }

    protected def updateWhen = Primes.prime31

    override def onTick_!() = {
      super.onTick_!()
      ifNth(updateWhen) {
        freeAlternativeTiles.prepareNextIfDone()
      }
    }

    protected val actionName = self.getClass.className

    override protected def wrapBase(unit: T) = new SingleUnitBehaviour[T](unit, meta) {

      private val isInstantFireUnit = unit.isInstantFireUnit

      private def weapon = unit.asInstanceOf[Weapon]

      override protected def butOnlyIf = {
        def couldFireInBetween = {
          isInstantFireUnit &&
          weapon.isReadyToFireWeapon &&
          weapon.hasTarget
        }
        super.butOnlyIf && !couldFireInBetween
      }

      override def describeShort = actionName

      override def toOrder(what: Objective): Seq[UnitOrder] = {
        freeAlternativeTiles.flatMapOnContent { result =>
          result.get(unit.currentTile)
        }.map { target =>
          Orders.MoveToTile(unit, target)
        }.toList
      }
    }
  }

  class MoveAwayFromConstructionSite(universe: Universe)
    extends AvoidSpecificAreas[Mobile](universe) {
    override protected def tilesToAvoidAsBlocked = mapLayers.blockedByPlannedBuildings.toSome

    override protected val actionName = "<>"

    override protected def debugColor = Color.White
  }

  class MoveAwayFromDangerousSpotOnAir(universe: Universe)
    extends AvoidSpecificAreas[AirUnit](universe) with DefaultDangerAreaConfig[AirUnit] {

    override protected def tilesToAvoidAsBlocked = {
      super.tilesToAvoidAsBlocked.map { base =>
        base.or(mapLayers.avoidanceSuggestionAir)
      }
    }

    override protected def debugColor = Color.Orange
  }

  trait DefaultDangerAreaConfig[T <: Mobile] extends AvoidSpecificAreas[T] {
    override protected def tolerance = 2
    override protected def targetReuseAllowance = 8
    override protected def updateWhen = Primes.prime5
    override protected val actionName: String = "<!>"
    override protected def tilesToAvoidAsBlocked = mapLayers.underPsiStorm.toSome
  }

  class MoveAwayFromDangerousSpotOnGround(universe: Universe)
    extends AvoidSpecificAreas[GroundUnit](universe) with DefaultDangerAreaConfig[GroundUnit] {

    override protected def tilesToAvoidAsBlocked = {
      super.tilesToAvoidAsBlocked.map { base =>
        base.or(mapLayers.avoidanceSuggestionGround)
      }
    }

    override protected def debugColor = Color.Red

  }

  class PreventBlockades(universe: Universe) extends DefaultBehaviour[Mobile](universe) {

    case class Feed(unitsToPositions: Map[Int, Area], baseArea: Grid2D, dangerous: Grid2D,
                    blockedByMobiles: Grid2D)

    def feed = {
      val baseArea = {
        mapLayers.freeWalkableTiles.mutableCopy.guaranteeImmutability
      }
      val blockedByMobiles = mapLayers.blockedByMobileUnitsExtended
      val unitsToPositions = ownUnits.allCompletedMobiles
                             .iterator
                             .filter(_.canMove)
                             .map { e => e.nativeUnitId -> e.blockedArea }
                             .toMap
      val buildingLayer = mapLayers.freeWalkableTiles.guaranteeImmutability
      val dangerous = mapLayers.slightlyDangerousForGroundAsBlocked.guaranteeImmutability
      Feed(unitsToPositions, baseArea, dangerous, blockedByMobiles)
    }

    private val unlockingPlan = FutureIterator.feed(feed).produceAsyncLater { in =>

      val operateOn = in.baseArea //.mutableCopy.or_!(in.blockedByMobiles.mutableCopy)

      val badlyPositioned = in.unitsToPositions.flatMap { case (id, where) =>
        val withOutline = where.growBy(tolerance)

        val evil = operateOn.cuttingAreas(withOutline)

        def safe = in.dangerous.free(where.centerTile)
        if (evil && safe) {
          Some(where.centerTile -> id)
        } else {
          None
        }
      }

      val unlockPositions = badlyPositioned.flatMap { case (tile, unitId) =>
        val moveTo = {
          val layer = in.baseArea
          def candidates = layer.spiralAround(tile, 45)
                           .drop(25)
                           .sliding(1, 5)
                           .flatten
                           .filter(_.distanceToIsMore(tile, 3))
                           .filter(layer.freeAndInBounds)

          candidates.find { e =>
            layer.freeAndInBounds(e.asArea.growBy(tolerance))
          }
        }
        moveTo.map { tile => unitId -> tile }
      }
      unlockPositions
    }.named("Make plan to unlock blockades")

    private val relevantLayer = oncePerTick {
      mapLayers.freeWalkableTiles.mutableCopy
      //.or_!(mapLayers.blockedByMobileUnitsExtended.mutableCopy)
      .guaranteeImmutability
    }

    override def forceRepeatedCommands: Boolean = false

    override def onTick_!() = {
      super.onTick_!()
      ifNth(Primes.prime59) {
        unlockingPlan.prepareNextIfDone()
      }
    }

    override def renderDebug_!(renderer: Renderer) = {
      super.renderDebug_!(renderer)
      unlockingPlan.onMostRecent { map =>
        map.foreach { case (unitId, whereTo) =>
          ownUnits.byId(unitId).foreach { unit =>
            renderer.in_!(Color.Red).indicateTarget(unit.centerTile, whereTo)
          }
        }
      }
    }

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {

      override def describeShort: String = "<->"

      override def toOrder(what: Objective): Seq[UnitOrder] = {
        val layer = relevantLayer.get
        unlockingPlan.flatMap { plan =>
          plan.get(unit.nativeUnitId).map {Orders.MoveToTile(unit, _)}
        }.filter { command =>
          val near = command.to.distanceToIsLess(unit.currentTile, 3)
          def stillProblematic = mapNth(Primes.prime37, true)(
            layer.cuttingAreas(unit.blockedArea.growBy(tolerance)))
          !near || stillProblematic
        }.map(_.toList)
        .getOrElse(Nil)
      }
    }

    private def tolerance = 1
  }

  class RepairDamagedUnit(universe: Universe) extends DefaultBehaviour[SCV](universe) {

    private val helper = new NonConflictingTargets[Mechanic, SCV](universe = universe,
      rateTarget = m => PriorityChain(m.percentageHPOk),
      validTargetTest = _.isDamaged,
      subAccept = (m, t) => m.currentArea == t.currentArea,
      subRate = (m, t) => PriorityChain(-m.currentTile.distanceSquaredTo(t.currentTile)),
      own = true,
      allowReplacements = true)

    override def priority = SecondPriority.EvenMore

    override def onTick_!() = {
      super.onTick_!()
      helper.onTick_!()
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

    private val helper = new NonConflictingTargets[TerranBuilding, SCV](universe = universe,
      rateTarget = m => PriorityChain(m.percentageHPOk),
      validTargetTest = _.isDamaged,
      subAccept = (m, t) => m.currentArea.contains(t.areaOnMap),
      subRate = (m, t) => PriorityChain(-m.currentTile.distanceSquaredTo(t.centerTile)),
      own = true,
      allowReplacements = true)

    override def priority = SecondPriority.EvenMore

    override def onTick_!() = {
      super.onTick_!()
      helper.onTick_!()
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

    private val area   = oncePerTick {
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

    override def priority = SecondPriority.Max

    override def onTick_!() = {
      super.onTick_!()
      helper.onTick_!()
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
          val sameArea = area.areInSameWalkableArea(unit.currentTile, building.centerTile)
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

    override def canControl(u: WrapsUnit): Boolean = {
      super.canControl(u) && u.isFigher
    }

    override def onTick_!(): Unit = {
      super.onTick_!()
    }

    override def renderDebug_!(renderer: Renderer) = {
      super.renderDebug_!(renderer)
      worldDominationPlan.allAttacks.foreach { plan =>
        plan.migrationPlan.foreach { p =>
          p.targetFormationTiles.foreach { tile =>
            renderer.in_!(Color.Cyan).drawCircleAround(tile)
          }
          renderer.in_!(Color.Red).drawStar(p.originalDestination, 3)
          renderer.in_!(Color.Green).drawStar(p.safeDestination, 2)
        }
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
            override protected def ferryDropTarget = {
              finalDestination
            }
          }

        case m => new DefaultMigrationBehaviour[Mobile](unit)
      }
      behaviour
    }

    private class DefaultMigrationBehaviour[+T <: Mobile](unit: T)
      extends SingleUnitBehaviour(unit, meta) {
      override def describeShort = "--> X"

      override protected def toOrder(what: Objective) = {
        worldDominationPlan.attackOf(unit).map { attack =>
          attack.suggestActionFor(unit).asOrder.toList
        }.getOrElse(Nil)
      }
    }

  }

  class ReallyReallyLazy(universe: Universe) extends DefaultBehaviour[Mobile](universe) {

    override def priority = SecondPriority.None

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {

      override def isNoopTask = true

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

    override def priority = SecondPriority.EvenMore

    override protected def wrapBase(unit: Tank) = new SingleUnitBehaviour[Tank](unit, meta) {

      override def preconditionOk = upgrades.hasResearched(TankSiegeMode)

      override def toOrder(what: Objective) = {
        val enemies = unit.surroundings.mediumEnemyGroundUnits
        val buildings = unit.surroundings.mediumEnemyBuildings

        def buildingInRange = buildings.exists(_.area.distanceTo(unit.currentTile) <= 11)

        def siegeableInRange = {
          buildingInRange || enemies.iterator.filterNot(_.isHarmlessNow).take(4).size >= 3
        }

        def anyCloseButNotTooClose = {
          enemies.exists { e =>
            !e.isHarmlessNow && e.centerTile.distanceToIsMore(unit.centerTile, 4)
          }
        }

        val botheredByMelee = unit.underAttackByMelee
        if (unit.isSieged) {
          val staySieged = {
            val hasTargets = buildingInRange || anyCloseButNotTooClose
            hasTargets && !botheredByMelee
          }
          if (staySieged) {
            Nil
          } else {
            Orders.TechOnSelf(unit, TankSiegeMode).toList
          }
        } else {
          if (siegeableInRange && !botheredByMelee) {
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

    override protected def wrapBase(unit: Mobile) = {
      new SingleUnitBehaviour[Mobile](unit, meta) {
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
  }

  class UseComsat(universe: Universe) extends DefaultBehaviour[Comsat](universe) {
    private val detectThese = ArrayBuffer.empty[Group[CanHide]]
    private val helpThese   = ArrayBuffer.empty[Group[CanDie]]

    override def onTick_!() = {
      super.onTick_!()
      detectThese ++= {
        mapNth(Primes.prime31, Seq.empty[Group[CanHide]]) {
          val dangerous = {
            enemies.allByType[CanHide]
            .iterator
            .filterNot(_.isExposed)
            .filter { cloaked =>
              ownUnits.allCompletedMobiles
              .exists(_.centerTile.distanceToIsLess(cloaked.centerTile, 8))
            }
            .toVector
          }

          val groups = GroupingHelper.groupTheseNow(dangerous, universe)
          groups.sortBy(-_.size)
        }
      }

      helpThese ++= {
        val attacked = {
          ownUnits.allCanDie
          .iterator
          .filterNot { u =>
            helpThese.exists(_.covers(u))
          }
          .filter(_.underAttackByCloaked)
        }

        val groups = GroupingHelper.groupTheseNow(attacked, universe)
        groups.sortBy(-_.size)
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
        } else if (helpThese.nonEmpty && comsat.canCastNow(ScannerSweep)) {
          val first = helpThese.remove(0)
          if (first.memberUnits.forall(_.underAttackByCloaked)) {
            Orders.ScanWithComsat(comsat, first.center).toList
          } else {
            Nil
          }
        } else {
          Nil
        }
      }
    }
  }

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
      val defense = helper.allOutsideNonBlacklisted
      val neutralResourceFields = universe.resourceFields.resourceAreas.filterNot { field =>
        universe.bases.isCovered(field)
      }.map {_.nearbyFreeTile}
      defense //++ neutralResourceFields
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
                  val freeTarget = {
                    on.spiralAround(unit.currentTile).filter(on.free)
                    .maxByOpt { where =>
                      def ownUnitsCost = {
                        universe.unitGrid.own.allInRange[GroundUnit](where, 5)
                        .view
                        .filter { e =>
                          !e.isInstanceOf[HasSpiderMines] && !e.isAutoPilot
                        }
                        .map(_.buildPrice)
                        .fold(Price.zero)(_ + _)
                      }
                      def enemyUnitsCost = {
                        universe.unitGrid.enemy.allInRange[GroundUnit](where, 5)
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

        override def preconditionOk: Boolean = {
          universe.upgrades.hasResearched(Upgrades.Terran.SpiderMines)
        }
      }
    }

    trait State

    case class DroppingMine(tile: MapTilePosition) extends State

    case object Idle extends State

  }

  class Scout(universe: Universe) extends DefaultBehaviour[ArmedMobile](universe) {

    override def renderDebug_!(renderer: Renderer) = {
      super.renderDebug_!(renderer)
      renderer.in_!(Color.Blue)
      plan.plans.foreach { plan =>
        plan.covered.grouped(2).foreach { seq =>
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
        scout.isInGame && toCheck.forall { resourceArea =>
          mapLayers.dangerousAsBlocked.freeAndInBounds(resourceArea.nearbyFreeTile) &&
          !bases.isCovered(resourceArea)
        }
      }

      val covered = toCheck.toSet

      private val remainingToCheck = mutable.ArrayBuffer.empty ++= toCheck.map(_.nearbyFreeTile)

      private val nextPath = {
        FutureIterator
        .feed((scout.currentTile, remainingToCheck.head, universe.pathfinders.safeFor(scout)))
        .produceAsync { case (from, to, pathfinder) =>
          pathfinder.findPathNow(from, to).map(_.toMigration)
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
                        scout.canSee(paths.originalDestination)
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

      case class RawScoutingPlan(sc: ScoutingCandidate, resourceAreaIds: List[Int],
                                 startHere: Int) {
        def resourceAreaIdsInOrder = startHere :: (resourceAreaIds filterNot (_ == startHere))
      }

      class Feed {
        private val resourceAreasUnscouted = {
          strategicMap.resources
          .filter { ra =>
            mapLayers.dangerousAsBlocked.free(ra.nearbyFreeTile)
          }
          .filterNot(coveredRightNow)
          .filterNot(bases.isCovered)
          .map { ra =>
            ra.uniqueId -> ra.nearbyFreeTile
          }
        }

        val tileToResourceAreaId = resourceAreasUnscouted.map(_.swap).toMap

        val leftToCover        = resourceAreasUnscouted.map(_._2)
        val map                = mapLayers.rawWalkableMap
        val scoutingCandidates = {
          ownUnits.allMobilesWithWeapons
          .flatMap(_.asGroundUnit)
          .filter(_.onGround)
          .map { e =>
            ScoutingCandidate(e.nativeUnitId, e.initialNativeType.topSpeed(), e.currentArea.get,
              e.currentTile)
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
              val bestPair = {
                if (pointsToCheck.size == 1) {
                  pointsToCheck.toList
                } else {
                  val checkUs = pointsToCheck.filterNot(coveredAlready).combinations(2)

                  val bestOption = checkUs.map { seq =>
                    val Seq(a, b) = seq
                    (a, b) -> {
                      in.pathfinder.findSimplePathNow(a, b).map(_.length)
                    }
                  }.filter(_._2.isDefined)
                                   .map { case (a, b) => a -> b.get }
                                   .minByOpt(_._2)
                                   .map { case ((from, to), _) =>
                                     List(from, to)
                                   }.getOrElse(Nil)

                  bestOption
                }
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
                RawScoutingPlan(bs._1, bestPair.map(in.tileToResourceAreaId),
                  in.tileToResourceAreaId(bs._2))
              }
            }
            Iterator.continually(findNextBestPairAndScouter()).takeWhile(_.isDefined).map(_.get)
            .toList

          }.getOrElse(Nil)
        }.toList
      }.named("Evaluate scouting plans")

      def onTick_!() = {
        val oldSize = scouts.size
        scouts.retain { (_, v) => v.valid }
        if (oldSize != scouts.size) {
          coveredRightNow.invalidate()
        }

        scouts.valuesIterator.foreach(_.onTick_!())

        leftToCover.onceIfDone { plans =>
          plans.foreach { plan =>
            ownUnits.byId(plan.sc.id).foreach { stillLiving =>
              val resourceAreas = plan.resourceAreaIdsInOrder.map(strategicMap.resourceAreaById)
              val startHere = strategicMap.resourceAreaById(plan.startHere)
              val unit = stillLiving.asInstanceOf[ArmedMobile]
              val actualPlan = new ScoutPlan(unit, resourceAreas)
              scouts.put(unit, actualPlan)
            }
          }
          if (plans.nonEmpty) {
            coveredRightNow.invalidate()
          }
        }
        ifNth(Primes.prime149) {
          leftToCover.prepareNextIfDone()
        }
      }

      def planFor(am: ArmedMobile) = {
        if (time.phase.isSinceAlmostMid) {
          scouts.get(am)
        } else {
          None
        }
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

    override def onTick_!() = {
      super.onTick_!()
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

    override def onTick_!() = {
      super.onTick_!()
      helper.onTick_!()
    }

    override def renderDebug_!(renderer: Renderer) = {
      super.renderDebug_!(renderer)
      helper.renderDebug_!(renderer)
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
  def renderDebug_!(renderer: Renderer): Unit = {
    mine2Enemy.foreach({ case (from, to) =>
      renderer.in_!(Color.White).drawLine(from.center, to.center)
    })
  }

  private val mine2Plan          = mutable.HashMap.empty[MobileRangeWeapon, Attackers]
  private val enemy2Mine         = mutable.HashMap.empty[CanDie, Attackers]
  private val mine2Enemy         = mutable.HashMap.empty[MobileRangeWeapon, CanDie]
  private val prioritizedTargets = {
    LazyVal.from {
      val prioritized = {
        universe.enemyUnits
        .allCanDie
        .iterator
        .filter(_.isAttackable)
        .toVector
        .sortBy { e =>
          (e.isHarmlessNow.ifElse(1, 0),
            enemy2Mine.contains(e).ifElse(0, 1),
            -e.price.sum,
            e.hitPoints.sum)
        }
      }
      prioritized
    }
  }

  private def consistent = {
    //    enemy2Mine.valuesIterator.foreach { attackers =>
    //      val m2e = attackers.allAttackers.map { e =>
    //        assert(mine2Enemy.contains(e))
    //        e -> mine2Enemy(e)
    //      }
    //      m2e.foreach { case (m,e) =>
    //        assert(mine2Enemy(m) == e)
    //      }
    //    }
    true
  }

  override def onTick_!() = {
    super.onTick_!()
    enemy2Mine.filter(_._1.isDead).foreach { case (dead, attackers) =>
      trace(s"Unit $dead died, reorganizing attackers")
      enemy2Mine.remove(dead)
      attackers.allAttackers.foreach { unit =>
        mine2Enemy.remove(unit)
      }
    }

    mine2Enemy.keySet.filter(_.isDead).foreach { dead =>
      val target = mine2Enemy.remove(dead)
      target.foreach { canDie =>
        enemy2Mine(canDie).removeAttacker_!(dead)
      }
    }

    enemy2Mine.valuesIterator.foreach(_.onTick_!())

    invalidateQueue()
  }

  def invalidateQueue(): Unit = {
    prioritizedTargets.invalidate()
  }

  def suggestTarget(myUnit: MobileRangeWeapon): Option[CanDie] = {
    val myCurrentTarget = mine2Enemy.get(myUnit)
    myCurrentTarget.foreach { t =>
      val maybeAttackers = enemy2Mine(t)
      val shouldLeaveTeam = {
        def outOfRange = maybeAttackers.isOutOfRange(myUnit)
        def overkill = maybeAttackers.isOverkill && maybeAttackers.canSpare(myUnit)
        outOfRange || overkill
      }
      if (shouldLeaveTeam) {
        maybeAttackers.removeAttacker_!(myUnit)
        mine2Enemy.remove(myUnit)
      }
    }

    val bestTarget = prioritizedTargets.find { target =>
      val existing = enemy2Mine.get(target).exists(_.isAttacker(myUnit))
      assert(!existing || myCurrentTarget.isDefined)
      existing || (myUnit.canAttackIfNear(target) && myUnit.isInWeaponRangeExact(target) &&
                   enemy2Mine.getOrElseUpdate(target, new Attackers(target)).canTakeMore)
    }
    bestTarget.foreach { attackThis =>
      val oldPlan = mine2Plan.get(myUnit)
      oldPlan.foreach { plan =>
        if (plan.isAttacker(myUnit)) {
          plan.removeAttacker_!(myUnit)
        }
        mine2Plan.remove(myUnit)
      }
      val plan = enemy2Mine(attackThis)
      if (plan.isAttacker(myUnit)) {
        assert(mine2Enemy.contains(myUnit))
      } else {
        mine2Enemy.put(myUnit, attackThis)
        plan.addAttacker_!(myUnit)
        mine2Plan.put(myUnit, plan)
      }
      invalidateQueue()
    }
    mine2Enemy.get(myUnit)
  }

  class Attackers(val target: CanDie) {

    private val attackers           = mutable.HashSet.empty[MobileRangeWeapon]
    private val plannedDamage       =
      mutable.HashMap.empty[MobileRangeWeapon, DamageSingleAttack]
    private val hpAfterNextAttacks  = currentHp
    private val plannedDamageMerged = new MutableHP(0, 0)

    def isOutOfRange(myUnit: MobileRangeWeapon) = !myUnit.isInWeaponRangeExact(target)

    def removeAttacker_!(t: MobileRangeWeapon): Unit = {
      assert(attackers(t))
      assert(plannedDamage.contains(t))
      attackers -= t
      plannedDamage -= t
      recalculatePlannedDamage_!()
      hpAfterNextAttacks.set(currentHp -! plannedDamageMerged)
      invalidateQueue()
    }

    def onTick_!(): Unit = {
      // adjust to reality
      hpAfterNextAttacks.set(currentHp -! plannedDamageMerged)
    }

    private def currentHp = new MutableHP(actualHP.hitpoints, actualHP.shield)

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
      assert(enemy2Mine(target) == this)
      attackers += t
      // for slow attacks, we assume that one is always on the way to hit to avoid overkill
      val expectedDamage = {
        val factor = 1 + t.assumeShotDelayOn(target)

        t.calculateDamageOn(target,
          hpAfterNextAttacks.hitPoints,
          hpAfterNextAttacks.shieldPoints,
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

    def isOverkill = actualHP <(plannedDamageMerged.hitPoints, plannedDamageMerged.shieldPoints)

    private def actualHP = target.hitPoints

    def canTakeMore = hpAfterNextAttacks.alive

    class MutableHP(var hitPoints: Int, var shieldPoints: Int) extends HasHpAndShields {
      override def hp = hitPoints

      override def shields = shieldPoints

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

    override def toString = s"Attackers($attackers)"
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

  private val validTarget        = (t: T) => t.isInGame && validTargetTest(t)
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

  override def onTick_!() = {
    super.onTick_!()
    val noLongerValid = assignments.filter { case (m, t) =>
      !m.isInGame || !validTarget(t)
    }
    noLongerValid.foreach { case (k, v) => unlock_!(k, v) }
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

  def lock_!(t: T, m: M): Unit = {
    assert(!locked(t))
    assert(!assignments.contains(m))

    assignments.put(m, t)
    assignmentsReverse.put(t, m)
    locks += t
  }

  private def locked(t: T) = locks(t)

  def unlock_!(m: M): Unit = {
    if (assignments.contains(m)) {
      val target = assignments(m)
      unlock_!(m, target)
    }
  }

  private def targetOf(m: M) = assignments(m)
}

class NonConflictingSpellTargets[T <: HasSingleTargetSpells, M <: Mobile : Manifest]
(spell: SingleTargetSpell[T, M], targetConstraint: PartialFunction[Mobile, M],
 keepLocked: M => Boolean, override val universe: Universe)
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

    val filtered = prioritizedTargets.filterNot(locked)
    filtered.find {_.currentPosition.distanceSquaredTo(caster.currentPosition) < range}
  }
}



