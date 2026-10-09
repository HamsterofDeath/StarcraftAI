package pony
package brain
package modules

import pony.brain.UnitRequest.CherryPickers

import scala.collection.mutable.ArrayBuffer

class ManageMiningAtGeysirs(universe: Universe)
    extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
  private val gatheringJobs = ArrayBuffer.empty[ManageMiningAtGeysir]
  def workerCapacity        = gatheringJobs.filter(_.keep).map(_.idealWorkerCount).sum

  override def onTick_!(): Unit = {
    val unattended = unitManager.bases.finishedBases.filterNot(_.mainBuilding.isFloating)
      .filter(base => !gatheringJobs.exists(_.covers(base)))
    unattended.foreach { base =>
      base.myGeysirs.filterNot(g => gatheringJobs.exists(_.targetGeysir == g)).map { geysir =>
        new ManageMiningAtGeysir(base, geysir)
      }.foreach { gatheringJobs += _ }
    }

    gatheringJobs.filterNot(_.keep).foreach(_.releaseMiners())
    gatheringJobs.retain(_.keep).foreach(_.onTick_!())
  }

  class ManageMiningAtGeysir(base: Base, geysir: Geysir) extends Employer[WorkerUnit](universe) {
    self =>
    val targetGeysir     = geysir
    val idealWorkerCount = 3 +
      (base.mainBuilding.area.distanceTo(geysir.area) / 3)
        .toInt
    private val workerCountBeforeWantingGas = this.universe.mapLayers
      .isOnIsland(base.mainBuilding.tilePosition)
      .ifElse(8, 14)
    private var refinery = Option.empty[Refinery]

    override def toString = s"GetGas@${geysir.tilePosition}"

    def keep = geysir.isInGame && base.mainBuilding.isInGame && !base.mainBuilding.isFloating &&
      base.myGeysirs.contains(geysir)
    def releaseMiners(): Unit = unitManager.allJobsByUnitType[WorkerUnit].filter(_.employer == this).foreach(_.fail_!())

    override def onTick_!(): Unit = {
      super.onTick_!()
      refinery = refinery.filter(_.isInGame)
      refinery match {
        case None =>
          if (ownUnits.allByType[WorkerUnit].size >= workerCountBeforeWantingGas) {
            def requestExists = unitManager.requestedConstructions[Refinery]
              .exists(_.customPosition.predefined.contains(geysir.tilePosition))
            def jobExists = unitManager.constructionsInProgress[Refinery]
              .exists(_.buildWhere == geysir.tilePosition)
            def findAndRememberRefinery() = refinery.orElse(ownUnits.allByType[Refinery]
              .find(
                _.tilePosition == geysir.tilePosition
              )
              .filterNot(_.isBeingCreated)
              .flatMap { refinery =>
                self.refinery = Some(refinery)
                self.refinery
              })

            if (!requestExists && !jobExists && findAndRememberRefinery().isEmpty) {
              val where = AlternativeBuildingSpot.fromPreset(geysir.tilePosition)
              requestBuilding(classOf[Refinery], customBuildingPosition = where)
              NativeMatchEvidence.trace(
                "refinery-request",
                s"geysir=${geysir.tilePosition} base=${base.mainBuilding.tilePosition} workers=${ownUnits.allByType[WorkerUnit].size}"
              )
            }
          }
        case Some(ref) =>
          val player = nativeGame.self()
          // a flooded gas bank needs minerals, not more gas: keep one worker and send the rest to the minerals
          val flooded = player.gas >= 800 && player.gas > 2 * player.minerals
          val wanted  = if (flooded) 1 else idealWorkerCount
          val missing = wanted - teamSize
          if (missing < 0)
            unitManager.allJobsByUnitType[WorkerUnit].filter(_.employer == this).take(-missing).foreach(_.fail_!())
          if (missing > 0) {
            val ofType = UnitJobRequest
              .idleOfType(self, classOf[WorkerUnit], missing, Priority.CollectGas)
              .withOnlyAccepting(_.isCarryingNothing)
            val result = unitManager.request(ofType)
            if (result.units.isEmpty && currentTick % (24 * 60) < 24)
              NativeMatchEvidence.trace(
                "gas-hire-none",
                s"geysir=${geysir.tilePosition} missing=$missing team=$teamSize result=${result.getClass.getSimpleName}"
              )
            result.units.foreach { freeWorker =>
              assignJob_!(new MineGasAtGeysir(freeWorker, geysir))
            }
          }
      }
    }

    def covers(base: Base) = this.base.mainBuilding == base.mainBuilding && base.myGeysirs.contains(geysir)

    class MineGasAtGeysir(worker: WorkerUnit, targetGeysir: Geysir)
        extends UnitWithJob[WorkerUnit](self, worker, Priority.ConstructBuilding)
        with Interruptable[WorkerUnit]
        with CanAcceptUnitSwitch[WorkerUnit]
        with FerrySupport[WorkerUnit]
        with PathfindingSupport[WorkerUnit] {
      private val freeNearGeysir = {
        mapLayers.rawWalkableMap.nearestFreeBlock(geysir.tilePosition, 1)
      }
      private val nearestReachableBase = oncePer(Primes.prime71) {
        bases.allBases.filter { b =>
          !b.mainBuilding.isFloating && worker.currentArea.contains(b.mainBuilding.areaOnMap) &&
          !ferryManager.sealedApart(worker.currentTile, b.mainBuilding.tilePosition)
        }.minByOpt { base =>
          base.mainBuilding.area.distanceTo(worker.currentTile)
        }
      }
      private var state: State = Idle

      override def copyOfJobForNewUnit(replacement: WorkerUnit) = new MineGasAtGeysir(replacement, targetGeysir)

      override def asRequest = {
        val picker = {
          CherryPickers.cherryPickWorkerByDistance[WorkerUnit](targetGeysir.centerTile)()
        }
        UnitJobRequest.idleOfType(employer, worker.getClass)
          .withRequest(_.withCherryPicker_!(picker))
      }

      override def couldSwitchInTheFuture = geysir.nonEmpty

      override def canSwitchNow = !worker.isCarryingGas && !worker.isGatheringGas

      override def jobHasFailedWithoutDeath = {
        super.jobHasFailedWithoutDeath || refinery.isEmpty || refinery.exists(_.isDead)
      }

      override def shortDebugString: String = state.getClass.className

      override def isFinished = false

      override def ordersForTick: Seq[UnitOrder] = {
        val (newState, order) = state match {
          case Idle if refinery.isDefined =>
            Gathering -> Orders.Gather(worker, refinery.get)
          case Gathering if worker.isGatheringGas || worker.isMagic =>
            Gathering -> Orders.NoUpdate(worker)
          case _ =>
            Idle -> Orders.NoUpdate(worker)
        }
        state = newState
        order.toSeq
      }

      override protected def pathTargetPosition = {
        if (worker.isCarryingGas) {
          nearestReachableBase.map(_.mainBuilding.centerTile)
        } else {
          geysir.centerTile.toSome
        }
      }

      override protected def ferryDropTarget = {
        freeNearGeysir.forNone {
          warn(s"No free tile around $geysir")
        }
        freeNearGeysir
      }

      trait State

      case object Idle extends State

      case object Gathering extends State

    }

  }

}
