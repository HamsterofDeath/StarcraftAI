package pony
package brain
package modules

import bwapi.Color
import pony.Upgrades.Terran.{GhostCloak, InfantryCooldown, SpiderMines, TankSiegeMode, WraithCloak}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class DefaultBehaviour[T <: Mobile : Manifest](override val universe: Universe)
  extends HasUniverse with HasLazyVals {
  private val unit2behaviour  = mutable.HashMap.empty[T, SingleUnitBehaviour[T]]
  private val controlledUnits = mutable.HashSet.empty[T]

  override def onTick(): Unit = {
    super.onTick()
    val remove = controlledUnits.filterNot(_.isInGame)
    controlledUnits --= remove
    unit2behaviour --= remove

  }

  def forceRepeatedCommands = false

  def priority = SecondPriority.Default

  def refuseCommandsForTicks = 0

  protected def meta = SingleUnitBehaviourMeta(priority, refuseCommandsForTicks, forceRepeatedCommands)

  def renderDebug(renderer: Renderer): Unit = {}

  def behaviourOf(unit: Mobile) = {
    ifControlsOpt(unit) {identity}
  }
  def ifControlsOpt[R](m: Mobile)(f: (SingleUnitBehaviour[T]) => R) = {
    ifControls(m, Option.empty[R])(e => Some(f(e)))
  }
  def ifControls[R](m: Mobile, ifNot: R)(f: (SingleUnitBehaviour[T]) => R) = {
    if (controls(m)) {
      f(unit2behaviour(assumeSafe(m)))
    } else {
      ifNot
    }
  }
  def controls(unit: Mobile) = {
    canControl(unit) && controlledUnits.contains(assumeSafe(unit))
  }
  def assumeSafe(unit: Mobile): T = unit.asInstanceOf[T]
  def canControl(u: WrapsUnit) = {
    manifest[T].runtimeClass.isInstance(u)
  }
  def add_!(u: WrapsUnit, objective: Objective) = {
    assert(canControl(u))
    val behaviour = wrapBase(u.asInstanceOf[T])
    controlledUnits += behaviour.unit
    unit2behaviour.put(behaviour.unit, behaviour)
  }
  def cast = this.asInstanceOf[DefaultBehaviour[Mobile]]

  protected def wrapBase(unit: T): SingleUnitBehaviour[T]

}

object Terran {
  def allBehaviours(universe: Universe): Seq[DefaultBehaviour[Mobile]] = {
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
                      new FerryService(universe) ::
                      new RepairDamagedUnit(universe) ::
                      new MoveAwayFromConstructionSite(universe) ::
                      new UntrapOwnUnits(universe) ::
                      /*
                                            new RepairDamagedBuilding ::
                                           new UseComsat ::
                                           new Scout ::
                                           new DoNotStray ::
                                           new HealDamagedUnit ::
                                           new FixMedicalProblem ::
                                           new BlindDetector ::
                      */
                      new Dance(universe) ::
                      new FocusFire(universe) ::
                      Nil).map(_.cast)
    allOfThem
  }

  class FerryService(universe: Universe) extends DefaultBehaviour[TransporterUnit](universe) {

    override def forceRepeatedCommands: Boolean = false

    override protected def wrapBase(unit: TransporterUnit) = new SingleUnitBehaviour[TransporterUnit](unit, meta) {

      override def forceRepeats: Boolean = true

      override def blocksForTicks: Int = 24

      override def shortName: String = "T"

      override def toOrder(what: Objective): Seq[UnitOrder] = {
        ferryManager.planFor(unit) match {
          case Some(plan) =>
            plan.toDropNow.map { dropThis =>
              Orders.UnloadUnit(unit, dropThis)
            }.orElse {
              if (plan.unloadedLeft) {
                val loadThis = plan.toTransport
                               .view
                               .filterNot(_.loaded)
                               .minBy(_.currentTile.distanceToSquared(unit.currentTile))
                Orders.LoadUnit(unit, loadThis).toSome
              } else if (plan.needsToReachTarget) {
                Orders.MoveToTile(plan.ferry, plan.toWhere).toSome
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
              val nearestFree = mapLayers.blockedByAnythingTiles.nearestFree(unit.currentTile)
              nearestFree.map { where =>
                Orders.UnloadAll(unit, where).forceRepeat_!(true)
              }
            } else if (unit.isPickingUp) {
              Some(Orders.Stop(unit))
            }
            else {
              None
            }).toList

        }
      }
    }
  }

  class Dance(universe: Universe) extends DefaultBehaviour[Mobile with Weapon](universe) {

    override def priority: SecondPriority = SecondPriority.More

    override def canControl(u: WrapsUnit): Boolean = super.canControl(u) && u.isInstanceOf[Weapon]

    override def refuseCommandsForTicks = 12 // to avoid wasting cpu time

    private val immutableMapLayer = oncePerTick {
      mapLayers.blockedByAnythingTiles.asReadOnlyCopyIfMutable
    }

    override protected def wrapBase(unit: Mobile with Weapon): SingleUnitBehaviour[Mobile with Weapon] = new
        SingleUnitBehaviour[Mobile with Weapon](unit, meta) {

      trait State
      case object Idle extends State
      case class Fallback(to: MapTilePosition, startedAtTick: Int) extends State

      private var state: State    = Idle
      private var runningCommands = List.empty[UnitOrder]
      private val beLazy          = (Idle, List.empty[UnitOrder])

      override def shortName: String = "D"
      override def toOrder(what: Objective) = {
        val (newState, newOrder) = {
          state match {
            case Idle =>
              val coolingDown = !unit.isReadyToFireWeapon
              lazy val dancePartners = {
                val allEnemies = universe.unitGrid.enemy.allInRange[Mobile](unit.currentTile,
                  unit.weaponRangeRadius / 32)
                allEnemies
                .collect { case enemy: Weapon if enemy.initialNativeType.topSpeed <= unit.initialNativeType.topSpeed &&
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
                  on.spiralAround(unit.currentTile, 8).slice(36, 186).filter { c =>
                    c.distanceToSquared(ref) <= unit.currentTile.distanceToSquared(ref)
                  }.find {on.free}
                  .orElse {
                    // second try: consider slipping through enemy lines
                    val center = MapTilePosition(xSumCenter / dpSize, ySumCenter / dpSize)
                    val map = on
                    val myArea = map.areaWhichContainsAsFree(unit.currentTile)
                    myArea.flatMap { a =>
                      on.spiralAround(unit.currentTile, 8).slice(36, 186)
                      .filter(map.free)
                      .filter(a.free)
                      .maxByOpt(center.distanceToSquared)
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
              if (unit.isReadyToFireWeapon || startedWhen + 24 < universe.currentTick || unit.currentTile == where) {
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

  class RepairDamagedBuilding(universe: Universe) extends DefaultBehaviour[SCV](universe) {
    override protected def wrapBase(t: SCV) = ???
  }


  class MoveAwayFromConstructionSite(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    private def tolerance = 2

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {
      override def shortName: String = "<>"
      override def toOrder(what: Objective): Seq[UnitOrder] = {
        val layer = mapLayers.blockedByPlannedBuildings
        val secondLayer = mapLayers.blockedByAnythingTiles
        val needsToMove = layer.anyBlocked(unit.blockedArea.growBy(tolerance))
        if (needsToMove) {
          layer.spiralAround(unit.currentTile).find { tile =>
            val extendedSizeToBeSafe = unit.unitTileSize.growBy(tolerance)
            layer.freeAndInBounds(tile, extendedSizeToBeSafe) &&
            secondLayer.freeAndInBounds(tile, extendedSizeToBeSafe)
          }.map { target =>
            Orders.MoveToTile(unit, target)
          }.toList
        } else {
          Nil
        }
      }
    }
  }

  class UntrapOwnUnits(universe: Universe) extends DefaultBehaviour[Mobile](universe) {

    private def tolerance = 1

    override def forceRepeatedCommands: Boolean = true

    private val newBlockingUnits = oncePer(Primes.prime37, {
      val baseArea = mapLayers.blockedByAnythingTiles.asReadOnlyCopyIfMutable
      val unitsToPositions = ownUnits.allCompletedMobiles
                             .filter(_.canMove)
                             .filterNot(_.isInstanceOf[WorkerUnit])
                             .map { e => e.nativeUnitId -> e.blockedArea }
                             .toMap
      BWFuture.some {
        val badlyPositioned = unitsToPositions.flatMap { case (id, where) =>
          val withOutline = where.growBy(tolerance)

          import scala.collection.breakOut
          val touched: Set[Grid2D] = withOutline.tiles.flatMap { tile =>
            if (baseArea.inBounds(tile)) baseArea.areaWhichContainsAsFree(tile) else None
          }(breakOut)

          if (touched.size > 1) {
            Some(where.upperLeft -> id)
          } else {
            None
          }
        }
        mutable.HashMap.empty ++= badlyPositioned
      }
    })

    private val blockingUnitsQueue = mutable.HashSet.empty[Mobile]

    universe.register_! { () =>
      newBlockingUnits.get.ifDoneOpt { locking =>
        val problems = locking.flatMap { case (_, id) =>
          ownUnits.byId(id).asInstanceOf[Option[Mobile]]
        }
        blockingUnitsQueue ++= problems
        if (problems.nonEmpty) {
          info(s"${problems.size} new units identified as area splitting")
        }
        debug(s"${blockingUnitsQueue.size} units identified as area splitting right now")
        locking.clear()
      }
    }

    private val relevantLayer = oncePerTick {
      mapLayers.blockedByAnythingTiles.asReadOnlyCopyIfMutable
    }

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {
      override def shortName: String = "<->"
      private var lastFreeGoTo = Option.empty[MapTilePosition]


      override def toOrder(what: Objective): Seq[UnitOrder] = {
        if (blockingUnitsQueue(unit)) {
          val layer = relevantLayer.get
          val command = lastFreeGoTo.filter(layer.free).map(Orders.MoveToTile(unit, _)).orElse {
            val safeArea = unit.blockedArea.growBy(tolerance+2)
            val moveTo = layer.spiralAround(unit.currentTile).find { e =>
              layer.outlineFreeAndInBounds(safeArea.moveTo(e))
            }
            moveTo.map { whereTo =>
              lastFreeGoTo = Some(whereTo)
              Orders.MoveToTile(unit, whereTo)
            }
          }.toList
          if (command.isEmpty || lastFreeGoTo.map(_.distanceTo(unit.currentTile)).getOrElse(0.0) < 1.5) {
            blockingUnitsQueue -= unit
          }
          command
        } else {
          Nil
        }
      }
    }
  }

  class RepairDamagedUnit(universe: Universe) extends DefaultBehaviour[SCV](universe) {

    private val helper = new NonConflictingTargets[Mechanic, SCV](universe = universe,
      rateTarget = m => PriorityChain(m.percentageHPOk),
      validTarget = _.isDamaged,
      subAccept = (m, t) => m.currentArea == t.currentArea,
      subRate = (m, t) => PriorityChain(-m.currentTile.distanceToSquared(t.currentTile)),
      own = true, allowReplacements = true)

    override protected def wrapBase(unit: SCV) = new SingleUnitBehaviour[SCV](unit, meta) {
      override def shortName: String = "R"
      override def toOrder(what: Objective): Seq[UnitOrder] = {
        helper.suggestTarget(unit).map { what =>
          Orders.Repair(unit, what)
        }.toList
      }
    }
  }

  class ContinueInterruptedConstruction(universe: Universe) extends DefaultBehaviour[SCV](universe) {
    override protected def wrapBase(unit: SCV) = ???
  }

  class MigrateTowardsPosition(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    override def priority = SecondPriority.Less

    override def canControl(u: WrapsUnit): Boolean = super.canControl(u) && !u.isInstanceOf[WorkerUnit]

    override def onTick(): Unit = {
      super.onTick()
    }

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit,
      meta) {
      override def shortName: String = "A"
      override def toOrder(what: Objective) = {
        worldDominationPlan.attackOf(unit).map { attack =>
          attack.suggestActionFor(unit).asOrder.toList
        }.getOrElse(Nil)
      }
    }
  }

  class StimSelf(universe: Universe) extends DefaultBehaviour[CanUseStimpack](universe) {
    override protected def wrapBase(unit: CanUseStimpack) = new SingleUnitBehaviour[CanUseStimpack](unit, meta) {

      override def preconditionOk = upgrades.hasResearched(InfantryCooldown)

      override def toOrder(what: Objective) = {
        if (unit.isAttacking && !unit.isStimmed) {
          List(Orders.TechOnSelf(unit, InfantryCooldown))
        } else {
          Nil
        }
      }
      override def shortName: String = s"SS"
    }
  }

  class SiegeUnsiegeSelf(universe: Universe) extends DefaultBehaviour[Tank](universe) {

    override def forceRepeatedCommands = true

    override protected def wrapBase(unit: Tank) = new SingleUnitBehaviour[Tank](unit, meta) {

      override def preconditionOk = upgrades.hasResearched(TankSiegeMode)

      override def toOrder(what: Objective) = {
        val trav = universe.unitGrid.enemy.allInRange[GroundUnit](unit.currentTile, 12)
        def enemiesNear = {
          trav.view.filter(!_.isHarmlessNow).take(4).size >= 3
        }
        def anyNear = {
          trav.exists(!_.isHarmlessNow)
        }

        if (unit.isSieged) {
          if (anyNear) {
            Nil
          } else {
            Orders.TechOnSelf(unit, TankSiegeMode).toList
          }
        } else {
          if (enemiesNear) {
            Orders.TechOnSelf(unit, TankSiegeMode).toList
          } else {
            Nil
          }
        }
      }
      override def shortName: String = s"SS"
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
      override def shortName: String = s"C"
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
      override def shortName: String = s"C"
    }
  }

  class GoToInitialPosition(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    private val helper = new FormationHelper(universe)

    private val ignore = mutable.HashSet.empty[Mobile]

    override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit,
      meta) {
      override def shortName: String = "IP"
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

  class UseComsat(universe: Universe) extends DefaultBehaviour[MobileDetector](universe) {
    override protected def wrapBase(t: MobileDetector) = ???
  }

  class SetupMineField(universe: Universe) extends DefaultBehaviour[Vulture](universe) {
    trait State
    case object Idle extends State
    case class DroppingMine(tile: MapTilePosition) extends State
    val beLazy = Idle -> Nil

    private val helper = new FormationHelper(universe, 2)

    private val plannedDrops = ArrayBuffer.empty[(Area, Int)]

    private val mined = oncePerTick {
      plannedDrops.retain(_._2 + 120 > universe.currentTick)
      val area = universe.mapLayers.freeWalkableTiles.mutableCopy
      universe.ownUnits.allByType[SpiderMine].foreach { mine =>
        area.block_!(mine.blockedArea.extendedBy(1))
        plannedDrops.foreach(e => area.block_!(e._1))
      }
      area
    }

    private def suggestMinePositions = {
      val defense = helper.allOutsideNonBlacklisted
      val neutralResourceFields = universe.resourceFields.resourceAreas.filterNot { field =>
        universe.bases.isCovered(field)
      }.map {_.mostAnnoyingMinePosition}
      defense //++ neutralResourceFields
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
      override def shortName: String = "LM"
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
                val freeTarget = on.spiralAround(unit.currentTile).filter(on.free).maxByOpt { where =>
                  def ownUnitsCost = {
                    universe.unitGrid.own.allInRange[GroundUnit](where, 5)
                    .view
                    .filter(e => !e.isInstanceOf[HasSpiderMines] && !e.isAutoPilot)
                    .map(_.buildPrice)
                    .fold(Price.zero)(_ + _)
                  }
                  def enemyUnitsCost = {
                    universe.unitGrid.enemy.allInRange[GroundUnit](where, 5)
                    .view
                    .filter(e => !e.isInstanceOf[HasSpiderMines] && !e.isAutoPilot)
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
                val dropMineHere = candiates.minByOpt(_.distanceToSquared(unit.currentTile))
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
      override def preconditionOk: Boolean = universe.upgrades.hasResearched(Upgrades.Terran.SpiderMines)
    }
  }

  class Scout(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    override protected def wrapBase(t: Mobile) = ???
  }

  class DoNotStray(universe: Universe) extends DefaultBehaviour[SupportUnit](universe) {
    override protected def wrapBase(t: SupportUnit) = ???
  }

  class OneTimeUnitSpellCast[C <: HasSingleTargetSpells : Manifest, T <: Mobile : Manifest](universe: Universe,
                                                                                            spell:
                                                                                            SingleTargetSpell[C, T])
    extends DefaultBehaviour[C](universe) {
    private val helper = NonConflictingSpellTargets.forSpell(spell, universe)

    override def refuseCommandsForTicks = 12

    override def priority: SecondPriority = SecondPriority.Max

    override protected def wrapBase(unit: C): SingleUnitBehaviour[C] = new SingleUnitBehaviour[C](unit, meta) {
      override def shortName: String = s"Cast ${spell.getClass.className}"
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
  class ShieldUnit(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.DefenseMatrix)
  class IrradiateUnit(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Irradiate)
  class CloakWraith(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Irradiate)

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

    override protected def wrapBase(unit: MobileRangeWeapon) = new
        SingleUnitBehaviour(unit, meta) {
      override def shortName = "FF"
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

  universe.register_!(() => {
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
  })

  universe.register_!(() => {
    invalidateQueue()
  })

  class Attackers(val target: MaybeCanDie) {
    def isOutOfRange(myUnit: MobileRangeWeapon) = !myUnit.isInWeaponRange(target)

    def recalculatePlannedDamage_!(): Unit = {
      val attackersSorted = plannedDamage.keys.toArray.sortBy(_.cooldownTimer)
      val damage = new MutableHP(0, 0)
      val currentHp = actualHP
      val assumeHp = new MutableHP(currentHp.hitpoints, currentHp.shield)
      attackersSorted.foreach { attacker =>
        val shotCount = 1 + attacker.assumeShotDelayOn(target)
        val moreDamage = attacker.calculateDamageOn(target, assumeHp.hp, assumeHp.shields, shotCount)
        damage +! moreDamage
        assumeHp -! moreDamage
      }
      plannedDamageMerged = damage
    }

    def removeAttacker_!(t: MobileRangeWeapon): Unit = {
      attackers -= t
      plannedDamage -= t
      recalculatePlannedDamage_!()
      invalidateQueue()
    }

    def canSpare(attacker: MobileRangeWeapon) = {
      assert(isAttacker(attacker))
      // this is not entirely correct because of shields & zerg regeneration, but should be close enough
      val damage = plannedDamage(attacker)

      actualHP <(plannedDamageMerged.hp - damage.onHp, plannedDamageMerged.shields - damage.onShields)
    }

    def allAttackers = attackers.iterator

    def isAttacker(t: MobileRangeWeapon) = attackers(t)

    def addAttacker_!(t: MobileRangeWeapon): Unit = {
      assert(!attackers(t), s"$attackers already contains $t")
      attackers += t
      // for slow attacks, we assume that one is always on the way to hit to avoid overkill
      val expectedDamage = {
        val factor = 1 + t.assumeShotDelayOn(target)

        t.calculateDamageOn(target, hpAfterNextAttacks.hp, hpAfterNextAttacks.shields, factor)
      }

      plannedDamage.put(t, expectedDamage)
      recalculatePlannedDamage_!()
      hpAfterNextAttacks -! expectedDamage
    }

    def isOverkill = actualHP <(plannedDamageMerged.hp, plannedDamageMerged.shields)

    class MutableHP(var hp: Int, var shields: Int) {
      def toHP = HitPoints(hp, shields)

      def +!(damageDone: DamageSingleAttack) = {
        hp += damageDone.onHp
        shields += damageDone.onShields
      }

      assert(shields >= 0)

      def alive = hp > 0

      def -!(dsa: DamageSingleAttack) = {
        hp -= dsa.onHp
        shields -= dsa.onShields
      }
    }

    private val attackers = mutable.HashSet.empty[MobileRangeWeapon]
    private def actualHP = target.hitPoints
    private val plannedDamage       = mutable.HashMap.empty[MobileRangeWeapon, DamageSingleAttack]
    private var plannedDamageMerged = new MutableHP(0, 0)
    private val hpAfterNextAttacks  = new
        MutableHP(actualHP.hitpoints, actualHP.shield)

    def canTakeMore = hpAfterNextAttacks.alive
  }

  def invalidateQueue(): Unit = {
    prioritizedTargets.invalidate()
  }
  private val enemy2Attackers    = mutable.HashMap.empty[MaybeCanDie, Attackers]
  private val me2Enemy           = mutable.HashMap.empty[MobileRangeWeapon, MaybeCanDie]
  private val prioritizedTargets = LazyVal.from {
    val prioritized = universe.enemyUnits.allCanDie.toVector.sortBy { e =>
      (e.isHarmlessNow.ifElse(1, 0), enemy2Attackers.contains(e).ifElse(0, 1), -e.price.sum, e.hitPoints.sum)
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
}

object NonConflictingSpellTargets {
  def forSpell[T <: HasSingleTargetSpells, M <: Mobile : Manifest](spell: SingleTargetSpell[T, M],
                                                                   universe: Universe) = {

    new NonConflictingSpellTargets(spell, {
      case x: M if spell.canBeCastOn(x) & spell.shouldActivateOn(x) => x
    },
      spell.isAffected, universe)
  }
}

class NonConflictingTargets[T <: WrapsUnit : Manifest, M <: Mobile : Manifest](override val universe: Universe,
                                                                               rateTarget: T => PriorityChain,
                                                                               validTarget: T => Boolean,
                                                                               subAccept: (M, T) => Boolean,
                                                                               subRate: (M, T) => PriorityChain,
                                                                               own: Boolean,
                                                                               allowReplacements: Boolean)
  extends HasUniverse {

  universe.register_!(() => {
    val noLongerValid = assignments.filter { case (m, t) =>
      !m.isInGame || !validTarget(t)
    }
    noLongerValid.foreach { case (k, v) => unlock_!(k, v) }
  })

  def unlock_!(m: M, target: T) = {
    assignments.remove(m)
    assignmentsReverse.remove(target)
    locks -= target
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

  private val locks              = mutable.HashSet.empty[T]
  private val assignments        = mutable.HashMap.empty[M, T]
  private val assignmentsReverse = mutable.HashMap.empty[T, M]

  private def locked(t: T) = locks(t)
  private def targetOf(m: M) = assignments(m)
  def lock_!(t: T, m: M): Unit = {
    assert(!locked(t))
    assert(!assignments.contains(m))

    assignments.put(m, t)
    assignmentsReverse.put(t, m)
    locks += t
  }

  private val targets = universe.oncePerTick {
    val on = if (own) ownUnits else enemies
    on.allByType[T].iterator
    .filter(validTarget)
    .map { e => e -> rateTarget(e) }
    .toVector
    .sortBy(_._2)
    .map(_._1)
  }
}

class NonConflictingSpellTargets[T <: HasSingleTargetSpells, M <: Mobile : Manifest](spell: SingleTargetSpell[T, M],
                                                                                     targetConstraint:
                                                                                     PartialFunction[Mobile, M],
                                                                                     keepLocked: M => Boolean,
                                                                                     override val universe: Universe)
  extends HasUniverse {
  def afterTick(): Unit = {
    prioritizedTargets.invalidate()
    locked.filterNot(keepLocked).foreach { elem =>
      unlock_!(elem)
    }
  }

  universe.register_!(() => {
    afterTick()
  })

  def notifyLock_!(t: T, target: M): Unit = {
    locked += target
    val tar = Target(t, target)
    lockedTargets += tar
    assignments.put(target, tar)
    prioritizedTargets.invalidate()
  }

  private def unlock_!(target: M): Unit = {
    locked -= target
    val old = assignments.remove(target).get
    lockedTargets -= old
    prioritizedTargets.invalidate()
  }

  private val locked        = mutable.HashSet.empty[M]
  private val lockedTargets = mutable.HashSet.empty[Target[M]]
  private val assignments   = mutable.HashMap.empty[M, Target[M]]

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

  def suggestTargetFor(caster: T): Option[M] = {
    // for now, just pick the first in range that is not yet taken
    val range = spell.castRangeSquare

    val filtered = prioritizedTargets.get.filterNot(locked)
    filtered.find {_.currentPosition.distanceToSquared(caster.currentPosition) < range}
  }
}



