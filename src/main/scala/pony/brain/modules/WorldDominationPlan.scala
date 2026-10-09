package pony
package brain
package modules

import pony.AttackPriorities.{AttackPriority, Highest, Lowest}

import scala.collection.mutable.ArrayBuffer

class WorldDominationPlan(override val universe: Universe) extends HasUniverse {

  private val attacks = ArrayBuffer
    .empty[Attack]
  private var planInProgress: BWFuture[Option[IncompleteAttacks]] = BWFuture(None)
  @volatile private var thinking                                  = false
  private var thinkingSince                                       = 0
  private val baseDefense                                         = new CampaignDefenseControl
  def baseDefenseActive                                           = baseDefense.pressure
  def setBaseDefensePressure(active: Boolean, recallCampaign: Boolean = true, detail: => String = ""): Unit = {
    val changed = active != baseDefense.pressure || active && recallCampaign != baseDefense.recallsCampaign
    baseDefense.setPressure(active)
    baseDefense.recallsCampaign = recallCampaign
    if (active && recallCampaign) attacks.retain(a => !a.campaign)
    if (changed) NativeMatchEvidence.trace("base-defense-pressure", s"active=$active recall=$recallCampaign $detail")
  }
  def campaignForce: Set[Mobile]                       = attacks.iterator.filter(_.campaign).flatMap(_.force).toSet
  def requestBaseDefense(where: MapTilePosition): Unit = {
    if (
      baseDefense.pressure &&
      (baseDefense.target.forall(_.distanceToIsMore(where, 6)) ||
        (!thinking && attacks.isEmpty))
    ) baseDefense.queue(where)
  }
  def immediateBaseDefenseOrder(unit: Mobile): Option[UnitOrder] = {
    if (!baseDefense.pressure || attackOf(unit).exists(_.migrationPlan.isDefined)) None
    // a raid home can hold leaves the campaign army on its way
    else if (!baseDefense.recallsCampaign && attackOf(unit).exists(_.campaign)) None
    // Carpet pairs hold their spread posts; the wall guard covers the main.
    else if (universe.pluginByType[CarpetSpread].postOf(unit.nativeUnitId).isDefined) None
    else baseDefense.target.map(where => Orders.AttackMove(unit, where))
  }

  def allAttacks                                               = attacks.toVector
  def planningInProgress                                       = thinking
  def campaignForceSize                                        = attacks.filter(_.campaign).map(_.force.size).sum
  private var campaignTarget                                   = Option.empty[MapTilePosition]
  def setCampaignTarget(target: Option[MapTilePosition]): Unit = {
    // Small retargets from fog updates must not cancel a running campaign attack.
    val meaningfullyChanged = (target, campaignTarget) match {
      case (Some(next), Some(prev)) => next.distanceToIsMore(prev, 8)
      case (next, prev)             => next != prev
    }
    if (meaningfullyChanged) attacks.retain(a => !a.campaign)
    campaignTarget = target
  }

  def initiateCampaignAttack(where: MapTilePosition, expeditionIds: Set[Int]): Boolean = {
    if (thinking || baseDefense.pressure) return false
    val employer         = new Employer[Mobile](universe)
    val busy             = attacks.flatMap(_.force).toSet
    def joins(m: Mobile) = !m.isBeingCreated && !busy(m) && (
      // medics go along to heal and to blind the defenders; they stay among the fighters
      m.isInstanceOf[Medic] ||
        expeditionIds(m.nativeUnitId) && m.isFigher && !m.isInstanceOf[WorkerUnit] &&
        !m.isInstanceOf[SupportUnit] && !m.isInstanceOf[TransporterUnit]
    )
    val req       = UnitJobRequest.idleOfType(employer, classOf[Mobile], 9999).withOnlyAccepting(joins)
    val available = unitManager.request(req, buildIfNoneAvailable = false).units.collect {
      case m: Mobile if joins(m) => m
    }.toVector.sortBy(_.nativeUnitId)
    if (!available.exists(_.isFigher)) false
    else {
      initiateAttack(where, available, Lowest, campaign = true)
      true
    }
  }

  def renderDebug(renderer: Renderer) = {
    allAttacks.foreach(_.renderDebug(renderer))
    allAttacks.groupBy(_.migrationPlan.map(_.safeDestination)).foreach {
      case (where, attacksSharingTarget) =>
        where.foreach { tile =>
          val (arrived, total) = attacksSharingTarget.foldLeft(0, 0)((acc, e) => {
            val (arrived, total)           = acc
            val (addToArrived, addToTotal) = e.meetingStats.getOrElse(0, 0)
            (arrived + addToArrived, total + addToTotal)
          })
          renderer.drawTextAtTile(s"Meeting point, ${arrived} of ${total}", tile)
        }
    }
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    // A stuck background plan must never freeze the campaign forever.
    if (!thinking) thinkingSince = currentTick
    else if (currentTick - thinkingSince > 24 * 120) {
      NativeMatchEvidence.trace("attack-plan-timeout", s"stuckSince=$thinkingSince")
      thinking = false
      planInProgress = BWFuture.none
    }
    attacks.foreach(_.onTick_!())
    attacks.retain(_.hasNotEnded)
    if (thinking) {
      planInProgress.result.foreach { plan =>
        attacks ++= plan.parts.filter(p =>
          !p.campaign ||
            (campaignTarget.exists(c => !c.distanceToIsMore(p.destination.where, 12)) &&
              baseDefense.acceptsCampaign(p.defenseGeneration))
        )
          .flatMap(_.complete)
        thinking = false
        planInProgress = BWFuture.none
        NativeMatchEvidence.trace(
          "attack-plan",
          s"frames=${currentTick - thinkingSince} parts=${plan.parts.size} campaign=${plan.parts.exists(_.campaign)}"
        )
        majorInfo(s"Attack plan finished!")
      }
    }
    baseDefense.takeReady(thinking).foreach { where =>
      NativeMatchEvidence.trace("base-defense-recall", s"$where campaign=${baseDefense.recallsCampaign}")
      // the highest priority clears every attack; a raid home can hold is met by the units not out on campaign
      initiateAttack(where, if (baseDefense.recallsCampaign) Highest else Lowest)
    }
  }

  def attackOf(m: Mobile) = attacks.find(_.force(m))

  def initiateAttack(where: MapTilePosition, priority: AttackPriority = Lowest): Unit = {
    majorInfo(s"Initiating attack of $where")
    if (thinking) return
    // this attack has priority over an existing one
    if (priority.isHigh) {
      attacks.clear()
    }

    val employer = new Employer[Mobile](universe)
    val req      = UnitJobRequest.idleOfType(employer, classOf[Mobile], 9999)
    val result   = unitManager.request(req, buildIfNoneAvailable = false)
    result.ifNotZero { seq =>
      val busy                = attacks.flatMap(_.force).toSet
      val notAlreadyAttacking = seq.collect { case m: Mobile if m.isFigher => m }
        .filterNot(busy)
      initiateAttack(where, notAlreadyAttacking, priority)
    }
  }

  def initiateAttack(where: MapTilePosition, units: Seq[Mobile], priority: AttackPriority): Unit =
    initiateAttack(where, units, priority, campaign = false)

  def initiateAttack(
      where: MapTilePosition,
      units: Seq[Mobile],
      priority: AttackPriority,
      campaign: Boolean
  ): Unit = {
    if (thinking || units.isEmpty) return
    debug(s"Attacking $where with $units")
    val helper            = new GroupingHelper(universe.mapLayers.rawWalkableMap, units, universe.allUnits)
    val defenseGeneration = baseDefense.generation
    planInProgress = BWFuture.produceFrom {
      val on         = universe.mapLayers.rawWalkableMap
      val grouped    = helper.evaluateUnitGroups
      val newAttacks = grouped.map { group =>
        val asUnits = {
          group.memberIds
            .flatMap { e =>
              val op = ownUnits.byId(e)
              op.forNone {
                warn(s"Cannot find unit with id $e aka ${e.toBase36}")
              }
              op.map(_.asInstanceOf[Mobile])
            }
        }

        new IncompleteAttack(asUnits.toSet, TargetPosition(where, 10), priority, campaign, defenseGeneration)
      }
      debug(s"Attack calculation finished, results: $newAttacks")
      IncompleteAttacks(newAttacks)
    }
    thinking = true
    // the timeout counts from here: a plan started right after another one timed out gets its full time
    thinkingSince = currentTick
  }

  trait Action {
    def asOrder: UnitOrder
  }

  case class MoveToPosition(who: Mobile, where: MapTilePosition) extends Action {
    override def asOrder = Orders.MoveToTile(who, where)
  }

  case class AttackToPosition(who: Mobile, where: MapTilePosition) extends Action {
    override def asOrder = Orders.AttackMove(who, where)
  }

  case class StayInPosition(who: Mobile) extends Action {
    override def asOrder = Orders.NoUpdate(who)
  }

  class IncompleteAttack(
      private var currentForce: Set[Mobile],
      targetOfAttack: TargetPosition,
      priority: AttackPriority,
      val campaign: Boolean,
      val defenseGeneration: Int
  ) {
    def destination = targetOfAttack
    def complete    = {
      val living = currentForce.filter(_.isInGame)
      if (living.isEmpty) None else Some(new Attack(living, targetOfAttack, campaign))
    }
  }

  class Attack(private var currentForce: Set[Mobile], targetOfAttack: TargetPosition, val campaign: Boolean)
      extends HasLazyVals {
    val uniqueId = WrapsUnit.nextId

    def meetingStats = migrationPlan.map(_.meetingStats)

    def force = currentForce

    def renderDebug(renderer: Renderer): Unit = {
      migrationPlan.foreach { plan =>
        plan.renderDebugPaths(renderer)
      }
    }

    private val area = {
      currentForce.map { unit =>
        mapLayers.rawWalkableMap
          .areaOf(unit.currentTile)
      }
        .groupBy(identity)
        .mapValuesStrict(_.size)
        .maxBy(_._2)
        ._1
    }

    assert(currentForce.nonEmpty, "WTF?")
    private val centerOfForce = this.oncePerTick {
      val realCenter = {
        currentForce.foldLeft(MapTilePosition.shared(0, 0))((acc, e) =>
          acc.movedByNew(e.currentTile)
        ) / currentForce.size
      }
      area.flatMap(_.nearestFree(realCenter))
        .getOrElse(realCenter)
    }
    private val pathToFollow = {
      universe.pathfinders.groundSafe
        .findPaths(currentCenter, targetOfAttack.where)
    }

    private val migration = pathToFollow.map(_.map(_.toMigration(using universe)))

    private val myTargetReachedPercentage = oncePer(Primes.prime11) {
      val reachedTargetPoint = migration.result.map { migration =>
        currentForce.count { m => migration.isCloseToUnsafeTarget(m) }
      }.getOrElse(0)

      reachedTargetPoint / currentForce.size.toDouble
    }

    def destination = targetOfAttack

    def migrationPlan = migration.result

    def hasNotEnded = !hasEnded

    def hasEnded = !hasMembers || (!campaign && allReachedTargetArea && halfOfForceReachedTargetPoint)

    def hasMembers = currentForce.nonEmpty

    def halfOfForceReachedTargetPoint = myTargetReachedPercentage > 0.5

    def allReachedTargetArea = migration.result.exists(_.allCloseToDestination)

    override def onTick_!(): Unit = {
      super.onTick_!()
      currentForce = currentForce.filter(_.isInGame)
      migration.result.foreach(_.onTick_!())
    }

    def currentCenter = centerOfForce.get

    def completePath = pathToFollow

    def suggestActionFor(t: Mobile) = {
      migration.result match {
        case None =>
          // no path calculated yet
          StayInPosition(t)
        case Some(path) =>
          def defaultCommand = {
            path.nextFor(t) match {
              case Some((targetTile, attackMove)) if attackMove =>
                AttackToPosition(t, targetTile)
              case Some((targetTile, attackMove)) =>
                MoveToPosition(t, targetTile)
              case None =>
                if (campaign) AttackToPosition(t, targetOfAttack.where) else StayInPosition(t)
            }
          }

          t match {
            case s: SupportUnit =>
              // among the nearest fighters of this attack; away from them, along the attack's path
              val stayHere = {
                val stayBetweenThese = {
                  s.nearestAllies.iterator
                    .filter(a => a.isFigher && currentForce(a) && a.currentTile.distanceToIsLess(s.currentTile, 8))
                    .take(3)
                    .map(_.currentTile)
                }
                MapTilePosition.averageOpt(stayBetweenThese)
              }
              stayHere.map { AttackToPosition(t, _) }.getOrElse { defaultCommand }
            case _ =>
              defaultCommand
          }
      }
    }

    override def toString = s"--> X $uniqueId@$destination, $meetingStats"

    override protected def currentTick = universe.currentTick
  }

  case class Attacks(parts: Seq[Attack])

  case class IncompleteAttacks(parts: Seq[IncompleteAttack]) {
    def complete = Attacks(parts.flatMap(_.complete))
  }

}
