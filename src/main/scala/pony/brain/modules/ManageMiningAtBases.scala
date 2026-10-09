package pony
package brain
package modules

import pony.brain.UnitRequest.CherryPickers

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ManageMiningAtBases(universe: Universe) extends OrderlessAIModule[WrapsUnit](universe) {

  private val gatheringJobs        = ArrayBuffer.empty[ManageMiningAtPatchGroup]
  private val opening              = new TerranEconomicProgress
  private var secondWasOperational = false
  def fieldStates                  = gatheringJobs.filter(g => g.natural && !g.forBase.mainBuilding.isFloating)
    .map(g =>
      MiningFieldStatus(
        g.forBase.resourceArea.get.uniqueId,
        g.capacity,
        g.teamSize,
        g.workingMiners,
        !g.forBase.mainBuilding.isBeingCreated && g.forBase.mainBuilding.isInGame
      )
    ).toVector
  def startingFieldSaturated = opening.startingFieldSaturated
  def secondBaseEstablished  = opening.secondBaseEstablished
  def workerCapacity = gatheringJobs.filter(g => g.natural && !g.forBase.mainBuilding.isFloating).map(_.capacity).sum

  /** The base whose mineral crew lacks the most workers. */
  def neediestBase: Option[Base] = gatheringJobs.iterator
    .filter(g => g.natural && !g.forBase.mainBuilding.isFloating && g.capacity > g.teamSize)
    .maxByOpt(g => g.capacity - g.teamSize).map(_.forBase)
  def servingMineralDepots(field: Int): Vector[MainBuilding] = {
    val employers = gatheringJobs.filter(g =>
      g.natural && g.attachedToBase &&
        g.forBase.resourceArea.exists(_.uniqueId == field)
    ).map(_.forBase.mainBuilding)
    val returningTo = unitManager.allJobsByUnitType[WorkerUnit].collect {
      case job: GatherMineralsAtSinglePatch if job.worker.isCarryingMinerals =>
        Option(job.worker.nativeUnit.getOrderTarget).map(_.getID)
    }.flatten.toSet
    val observed = bases.finishedBases.filter(b =>
      !b.mainBuilding.isFloating &&
        b.resourceArea.exists(_.uniqueId == field) && returningTo(b.mainBuilding.nativeUnitId)
    )
      .map(_.mainBuilding)
    (employers ++ observed).distinct.toVector
  }

  override def onTick_!(): Unit = {
    val detached = gatheringJobs.filterNot(g => g.attachedToBase && g.permittedStaffing)
    detached.foreach(_.releaseMiners())
    gatheringJobs --= detached
    createJobsForBases()
    ifNth(Primes.prime43) {
      val outdated = gatheringJobs.filter(_.unnatural).flatMap { e =>
        val base            = e.forBase
        val currentDistance = base.mainBuilding.centerTile.distanceSquaredTo(e.patchGroup.anyTile)
        val nearest         = universe.bases
          .finishedBases
          .iterator
          .filterNot(b => b == base || b.mainBuilding.isFloating)
          .minByOpt(_.mainBuilding.centerTile.distanceSquaredTo(e.patchGroup.anyTile))
        nearest.flatMap { bestReplacement =>
          val altDistance = bestReplacement.mainBuilding.centerTile
            .distanceSquaredTo(e.patchGroup.anyTile)

          if (altDistance < currentDistance) {
            Some(e)
          } else {
            None
          }
        }
      }
      debug(s"Replacing ${outdated.size} outdated mining jobs")
      outdated.foreach(_.releaseMiners())
      gatheringJobs --= outdated
    }
    gatheringJobs.foreach(_.onTick_!())
    if (universe.currentTick < 3000 && (gatheringJobs.isEmpty || gatheringJobs.map(_.teamSize).sum == 0))
      NativeMatchEvidence.trace(
        "mining-scan",
        s"jobs=${gatheringJobs.size} staffed=${gatheringJobs.map(_.teamSize).sum} detachedNow=${detached.size} first=" +
          gatheringJobs.headOption.map(g => s"attached=${g.attachedToBase} permitted=${g.permittedStaffing}").getOrElse(
            "none"
          )
      )
    val before = opening.startingFieldSaturated
    opening.observe(bases.mainBase.flatMap(_.resourceArea).map(_.uniqueId), fieldStates)
    if (!before && opening.startingFieldSaturated)
      NativeMatchEvidence.trace("starting-field-saturated", fieldStates.mkString(";"))
    val operational = secondBaseEstablished
    if (operational && !secondWasOperational)
      NativeMatchEvidence.trace("second-field-mining", fieldStates.mkString(";"))
    secondWasOperational = operational
  }

  private def createJobsForBases(): Unit = {
    val add = {
      val naturals = {
        universe.bases
          .finishedBases
          .filterNot(_.mainBuilding.isFloating)
          .groupBy(_.resourceArea).values.map(_.minBy(_.mainBuilding.nativeUnitId)).toVector
          .filterNot(e => gatheringJobs.exists(_.covers(e)))
          .flatMap { base =>
            base.myMineralGroup.map { minerals =>
              new ManageMiningAtPatchGroup(base, minerals)
            }
          }
      }
      val unnaturals = if (strategy.current.runsTerranCampaign) Nil
      else {
        val poor = gatheringJobs.groupBy(_.forBase)
          .filter(_._2.forall(_.poor))

        val newTargets = poor.flatMap { case (base, jobs) =>
          base.alternativeResourceAreas.find { area =>
            !jobs.exists(e => area.patches.contains(e.patchGroup))
          }.map { e => base -> e }
        }.filter(_._2.patches.isDefined)

        newTargets.map { case (base, area) =>
          new ManageMiningAtPatchGroup(base, area.patches.get)
        }
      }

      naturals ++ unnaturals
    }
    info(
      s"""
         |Added new mineral gathering job(s): ${add.mkString(" & ")}
       """.stripMargin,
      add.nonEmpty
    )
    gatheringJobs ++= add
  }

  class ManageMiningAtPatchGroup(base: Base, minerals: MineralPatchGroup)
      extends Employer[WorkerUnit](universe) {
    emp =>

    private val boundField = base.resourceArea.map(_.uniqueId)
    def attachedToBase     = base.mainBuilding.isInGame && !base.mainBuilding.isFloating &&
      base.resourceArea.map(_.uniqueId) == boundField
    def permittedStaffing = MineralFieldStaffing.permitted(
      strategy.current.runsTerranCampaign,
      base.myMineralGroup.map(_.patchId),
      minerals.patchId
    )

    def natural = base.resourceArea.exists(_.patches.contains(minerals))

    def unnatural = !natural

    def patchGroup = minerals

    def poor = minerals.remainingPercentage <= 0.1

    def forBase       = base
    def capacity      = Micro.MiningOrganization.idealNumberOfWorkers
    def workingMiners = unitManager.allJobsByUnitType[WorkerUnit].count {
      case job: GatherMineralsAtSinglePatch => minerals.patches.contains(job.targetPatch) &&
        job.worker.isInGame && LocalMineralMining.observed(
          job.worker.isInMiningProcess,
          job.targetPatch.nativeUnitId,
          Option(job.worker.nativeUnit.getOrderTarget).map(_.getID),
          job.worker.centerTile.distanceToIsLess(job.targetPatch.centerTile, 4)
        )
      case _ => false
    }

    override def onTick_!(): Unit = {
      super.onTick_!()
      minerals.tick()
      Micro.MiningOrganization.onTick()
      val missing = idealNumberOfWorkers - teamSize
      if (missing > 0) {
        // Only hire workers that can actually walk to this field. A worker hired across a sealed
        // choke (our own wall) strands itself in a FAIL loop and occupies a team slot forever, so
        // the base would never ask for or receive local miners. Leaving the request unfulfilled
        // instead makes ProvideNewUnits train the missing worker here.
        val walkable = mapLayers.freeWalkableIgnoringMobiles
        // Resolve the anchor fresh every tick: nearbyFreeTile is cached once and a wall depot or
        // its blueprint can later cover it, which would silently reject every candidate. A clear
        // five-tile block also keeps the anchor out of pockets enclosed by the mineral line.
        val fieldAnchor = base.resourceArea.flatMap(a => walkable.nearestFreeBlock(a.center, 2))
        if (currentTick < 600) NativeMatchEvidence.trace(
          "mining-locality",
          s"anchor=$fieldAnchor anchorArea=${fieldAnchor.map(t => walkable.areaOf(t).isDefined)} " +
            ownUnits.allByType[WorkerUnit].take(5).map(w =>
              s"#${w.nativeUnitId}@${w.currentTile} area=${walkable.areaOf(w.currentTile).isDefined} " +
                s"same=${fieldAnchor.exists(t => walkable.areInSameWalkableArea(w.currentTile, t))}"
            ).mkString("|")
        )
        val result = this.universe.unitManager
          .request(UnitJobRequest.idleOfType(emp, classOf[WorkerUnit], missing)
            .withOnlyAccepting { worker =>
              fieldAnchor.exists(tile =>
                walkable.areInSameWalkableArea(worker.currentTile, tile) ||
                  ferryManager.sealedApart(worker.currentTile, tile) && unitManager.jobOf(worker).isIdleOrDefault
              )
            }.withRequest { r =>
              // behind a sealed wall an idle worker crosses by dropship; the nearest are taken first
              fieldAnchor.foreach(t => r.withCherryPicker_!(CherryPickers.cherryPickWorkerByDistance[WorkerUnit](t)()))
              r.trainNear_!(base.mainBuilding.tilePosition)
            })
        if (this.universe.currentTick < 3000)
          NativeMatchEvidence.trace(
            "mining-hire",
            s"missing=$missing team=$teamSize result=${result.getClass.getSimpleName} units=${result.units.size}"
          )
        val jobs = result.units.flatMap { worker =>
          Micro.MiningOrganization.findBestPatch(worker).map { patch =>
            info(s"Added $worker to mining team of $patch")
            val job = new Micro.MineMineralsAtPatch(worker, patch)
            patch.lockToPatch_!(job)
            NativeMatchEvidence.trace(
              "mining-assign",
              s"worker=${worker.nativeUnitId} patch=${patch.patch.nativeUnitId} hist=" +
                worker.asInstanceOf[OrderHistorySupport].unitHistory.take(3).map(_.order.toString).mkString(",")
            )
            job
          }
        }
        jobs.foreach(assignJob_!)
      }
    }

    private def idealNumberOfWorkers = Micro.MiningOrganization.idealNumberOfWorkers

    def covers(aBase: Base)   = base == aBase
    def releaseMiners(): Unit = unitManager.allJobsByUnitType[WorkerUnit].filter(_.employer == this).foreach(_.fail_!())

    override def toString = s"Gathering $minerals at $base"

    object Micro {

      class MinedPatch(val patch: MineralPatch) {

        private val miningTeam            = ArrayBuffer.empty[MineMineralsAtPatch]
        private val workerCountByDistance = LazyVal.from {
          val distance = math.round(patch.area.distanceTo(base.mainBuilding.area)).toInt
          distance match {
            case 0             => 1
            case 1 | 2 | 3 | 4 => 2
            case 5 | 6 | 7     => 3
            case x             => (x / 2.5).toInt
          }
        }

        override def toString: String = s"(Mined) $patch"

        def hasOpenSpot: Boolean = miningTeam.size < estimateRequiredWorkers

        def openSpotCount = estimateRequiredWorkers - miningTeam.size

        def estimateRequiredWorkers = {
          if (patch.remainingMinerals > 0) workerCountByDistance.get else 0
        }

        def lockToPatch_!(job: MineMineralsAtPatch): Unit = {
          info(s"Added ${job.unit} to mining team of $patch")
          miningTeam += job
        }

        def isInTeam(worker: WorkerUnit) = miningTeam.exists(_.unit == worker)

        def removeFromPatch_!(worker: WorkerUnit): Unit = {
          info(s"Removing $worker from mining team of $patch")
          val found = miningTeam.find(_.unit == worker)
          assert(found.isDefined, s"Did not find $worker in $miningTeam")
          found.foreach { miningTeam -= _ }
        }
      }

      class MineMineralsAtPatch(myWorker: WorkerUnit, miningTarget: MinedPatch)
          extends UnitWithJob(emp, myWorker, Priority.Default)
          with GatherMineralsAtSinglePatch
          with CanAcceptUnitSwitch[WorkerUnit]
          with Interruptable[WorkerUnit]
          with FerrySupport[WorkerUnit]
          with PathfindingSupport[WorkerUnit] {

        listen_!(failed => {
          if (miningTarget.isInTeam(myWorker)) {
            miningTarget.removeFromPatch_!(myWorker)
          }
        })

        import States._

        private val nearestReachableBase = oncePer(Primes.prime71) {
          bases.allBases.filter { b =>
            !b.mainBuilding.isFloating && worker.currentArea.contains(b.mainBuilding.areaOnMap) &&
            !ferryManager.sealedApart(worker.currentTile, b.mainBuilding.tilePosition)
          }.minByOpt { base =>
            base.mainBuilding.area.distanceTo(worker.currentTile)
          }
        }
        private var state: State = Idle

        override def copyOfJobForNewUnit(replacement: WorkerUnit) = new MineMineralsAtPatch(replacement, miningTarget)

        override def asRequest = {
          val picker = {
            CherryPickers.cherryPickWorkerByDistance[WorkerUnit](miningTarget.patch.centerTile)()
          }
          UnitJobRequest.idleOfType(emp, myWorker.getClass)
            .withRequest(_.withCherryPicker_!(picker))
        }

        override def couldSwitchInTheFuture = miningTarget.patch.hasRemainingMinerals

        override def canSwitchNow = !worker.isWaitingForMinerals &&
          !worker.isCarryingMinerals &&
          !worker.isInMiningProcess

        override def onStealUnit(): Unit = {
          super.onStealUnit()
          miningTarget.removeFromPatch_!(myWorker)
        }

        override def requiredWorkers: Int = miningTarget.estimateRequiredWorkers

        override def shortDebugString: String = state match {
          case States.Idle                               => s"Idle/${unit.nativeUnit.getOrder}"
          case States.ApproachingMinerals                => "Locked"
          case States.Mining                             => "Mining"
          case States.ReturningMinerals                  => "Delivering"
          case States.ReturningMineralsAfterInterruption => "Delivering (really)"
        }

        override def ordersForTick: Seq[UnitOrder] = {
          def sendWorkerToPatch = ApproachingMinerals -> Orders.Gather(myWorker, miningTarget.patch)
          def returnDelivery    = Orders.ReturnMinerals(myWorker, base.mainBuilding)
          if (myWorker.isGuarding) {
            state = Idle
          }
          def noop = Orders.NoUpdate(myWorker)
          // TODO use speed cheat
          val (newState, order) = state match {
            case Idle if myWorker.isCarryingMinerals =>
              ReturningMineralsAfterInterruption -> returnDelivery
            // start going to your patch using wall hack
            case Idle if !myWorker.isCarryingMinerals =>
              sendWorkerToPatch
            // keep going while mining has not been started

            case ApproachingMinerals if miningTarget.patch.isBeingMined =>
              // repeat the order to prevent the worker from moving away
              sendWorkerToPatch

            case ApproachingMinerals
                if myWorker.isWaitingForMinerals ||
                  myWorker.isInMiningProcess =>
              // let the poor worker alone now
              Mining -> noop

            case ApproachingMinerals =>
              ApproachingMinerals -> noop

            case Mining if myWorker.isInMiningProcess =>
              // let it work
              Mining -> noop
            case Mining if myWorker.isCarryingMinerals =>
              // the worker is done mining
              ReturningMinerals -> returnDelivery

            case ReturningMineralsAfterInterruption
                if myWorker.isCarryingMinerals &&
                  !myWorker.isMoving =>
              noCommandsForTicks_!(10)
              ReturningMineralsAfterInterruption -> returnDelivery
            case ReturningMineralsAfterInterruption if myWorker.isCarryingMinerals =>
              noCommandsForTicks_!(10)
              ReturningMinerals -> returnDelivery

            case ReturningMinerals if myWorker.isCarryingMinerals =>
              ReturningMinerals -> noop
            case ReturningMinerals if !myWorker.isCarryingMinerals =>
              // switch back to mining mode
              sendWorkerToPatch

            case _ =>
              Idle -> noop
          }
          state = newState
          order.toList.filterNot(_.isNoop)
        }

        override def isFinished = !miningTarget.patch.isInGame

        override def failedOrObsolete: Boolean = super.failedOrObsolete || base.mainBuilding.isDead

        override protected def pathTargetPosition = {
          if (worker.isCarryingMinerals) {
            nearestReachableBase.map(_.mainBuilding.centerTile)
          } else {
            miningTarget.patch.centerTile.toSome
          }
        }

        override def worker = myWorker

        override protected def ferryDropTarget = {
          targetPatch.tilePosition.middleBetween(base.mainBuilding.tilePosition).toSome
        }

        override def targetPatch = miningTarget.patch
      }

      object MiningOrganization {
        private val assignments = mutable.HashMap.empty[MineralPatch, MinedPatch]

        minerals.patches.foreach { mp =>
          assignments.put(mp, new MinedPatch(mp))
        }

        def onTick(): Unit = {
          val remove = assignments.keysIterator.filterNot(_.isInGame).toSet
          assignments --= remove
        }

        def findBestPatch(worker: WorkerUnit) = {
          val maxFreeSlots = assignments.iterator.map(_._2.openSpotCount).max
          val notFull      = assignments.filter(_._2.openSpotCount == maxFreeSlots)
          if (notFull.nonEmpty) {
            val (_, patch) = notFull.minBy { case (mins, _) =>
              mins.area.closestDirectConnection(worker.blockedArea).length
            }
            Some(patch)
          } else
            None
        }

        def idealNumberOfWorkers = assignments.valuesIterator.map(_.estimateRequiredWorkers).sum

      }

      object States {

        sealed trait State

        case object Idle extends State

        case object ApproachingMinerals extends State

        case object Mining extends State

        case object ReturningMinerals extends State

        case object ReturningMineralsAfterInterruption extends State

      }

    }

  }

}
