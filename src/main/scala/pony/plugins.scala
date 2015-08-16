package pony

import java.text.DecimalFormat

import bwapi.Color
import pony.Orders.AttackMove
import pony.brain.modules.GatherMineralsAtSinglePatch
import pony.brain.{HasUniverse, TwilightSparkle, UnitWithJob, Universe}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

class MapReveal extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    debugger.chat("black sheep wall")
  }
}

class FastSpeed extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    debugger.faster()
  }
}

class ChokePointRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Green)
      strategicMap.domains.get.foreach { case (choke, _) =>
        choke.lines.foreach { line =>
          renderer.drawLine(line.absoluteFrom, line.absoluteTo)
          renderer.drawTextAtTile(s"Chokepoint ${choke.index}", line.center)
        }
      }
    }
  }
}

class WalkableRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Red)

      val walkable = lazyWorld.map.walkableGrid
      walkable.all
      .filter(walkable.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
  }
}

class BuildableRenderer(ignoreWalkable: Boolean) extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Grey)

      val builable = lazyWorld.map.buildableGrid
      builable.allBlocked
      .filter { e => !ignoreWalkable || !lazyWorld.map.walkableGrid.blocked(e) }
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
  }
}

class UnitIdRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Green)

      lazyWorld.units.mineByType[Mobile].foreach { u =>
        renderer.drawTextAtMobileUnit(u, u.unitIdText)
      }
    }
  }
}

class UnitJobRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      unitManager.allJobsByUnitType[Mobile].foreach { job =>
        renderer.drawTextAtMobileUnit(job.unit, s"${job.shortDebugString} -> ${job.unit.nativeUnit.getOrder}", 1)
      }
    }
  }
  override def lazyWorld: DefaultWorld = universe.world
}

class StatsRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val lazyWorld       = universe.world

  override protected def tickPlugIn(): Unit = {

    lazyWorld.debugger.debugRender { renderer =>
      val debugString = ArrayBuffer.empty[String]

      debugString += {
        val locked = resources.lockedResources
        val locks = resources.detailledLocks
        s"${locked.minerals}m, ${locked.gas}g, ${locked.supply}s locked, ${locks.size} locks"
      }

      debugString += {
        val locked = unitManager.plannedToBuild.groupBy(_.typeOfRequestedUnit).map { case (k, v) =>
          s"${k.className}*${v.size}"
        }

        s"Planned (funded): ${locked.toList.sorted.mkString(", ")}"
      }

      debugString += {
        val locked = resources.failedToProvide.groupBy(_.whatFor).map { case (k, v) =>
          s"${k.className}*${v.size}"
        }

        s"In queue (no funds): ${locked.toList.sorted.mkString(", ")}"
      }

      debugString += {

        val missingUnits = unitManager.failedToProvideFlat.groupBy(_.typeOfRequestedUnit).mapValues(_.size)
        val formatted = missingUnits.map { case (unitClass, howMany) => s"${unitClass.className}/$howMany" }
        s"Type/missing: ${formatted.toList.sorted.mkString(", ")}"
      }

      val df = new DecimalFormat("#0.00")

      debugString ++= {
        universe.bases.bases.flatMap { base =>
          base.myMineralGroup.map { mins =>
            val gatherJob = unitManager.allJobsByType[GatherMineralsAtSinglePatch]
                            .filter(e => mins.contains(e.targetPatch))
            val stats = resources.stats
            val minsGot = stats.minerals
            val minsGotPerWorker = df.format(minsGot.toDouble / gatherJob.size)
            s"Base ${base.mainBuilding.unitIdText}: ${mins.value}m, ${
              gatherJob.size
            } workers, $minsGot income ($minsGotPerWorker avg)"
          }
        }.toList.sorted
      }

      debugString += {
        val formatted = unitManager.jobsByType.map { case (jobType, members) =>
          s"${jobType.className}/${members.size}"
        }
        s"Job/units: ${formatted.toList.sorted.mkString(", ")}"
      }

      debugString ++= {
        unitManager.employers.map { emp =>
          s"$emp has ${unitManager.jobsOf(emp).size} units"
        }.toList.sorted
      }



      debugString.zipWithIndex.foreach { case (txt, line) => renderer.drawTextOnScreen(txt, line) }
    }
  }
}

class BlockedBuildingSpotsRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val lazyWorld = universe.world

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Orange)
      val area = mapLayers.blockedByBuildingTiles
      area.allBlocked
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.White)
      val area = mapLayers.blockedByPlannedBuildings
      area.allBlocked
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Blue)
      val area = mapLayers.blockedByResources
      area.allBlocked
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Grey)
      val area = mapLayers.blockedByWorkerPaths
      area.allBlocked
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }

    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Grey)
      val area = mapLayers.blockedByMobileUnits
      area.allBlocked
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
  }
}

class MineralDebugRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val lazyWorld = universe.world

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Yellow)
      world.mineralPatches.groups.foreach { mpg =>
        mpg.patches.foreach { mp =>
          renderer.writeText(mp.tilePosition, s"#${mpg.patchId}")
        }
      }

      universe.unitManager.allJobsByType[GatherMineralsAtSinglePatch].groupBy(_.targetPatch).foreach { case (k, v) =>
        val estimatedWorkerCount = v.head.requiredWorkers
        renderer.drawTextAtStaticUnit(v.head.targetPatch, estimatedWorkerCount.toString, 1)
      }

    }
  }
}

class DebugHelper(main: AIAPIEventDispatcher with HasUniverse) extends AIPlugIn with HasUniverse {

  main.listen_!(new AIAPI {
    override def world: DefaultWorld = main.world
    override def onSendText(s: String): Unit = {
      super.onSendText(s)
      val words = s.split(' ').toList
      Try(words match {
        case command :: params =>
          command match {
            case "debug" =>
              params match {
                case List(id) =>
                  unitManager.jobByUnitIdString(id).foreach {debugUnit}
              }
            case "attack" =>
              params match {
                case List("minerals", id) =>
                  world.mineralPatches.groups.find(_.patchId.toString == id).foreach { group =>
                    units.mineByType[Mobile].filterNot(_.isInstanceOf[WorkerUnit]).foreach { u =>
                      world.orderQueue.queue_!(new AttackMove(u, group.center.randomized(12)))
                    }
                  }

                case List("choke", id) =>
                  strategicMap.domains.get.find(_._1.index.toString == id).foreach { choke =>
                    units.mineByType[Mobile].filterNot(_.isInstanceOf[WorkerUnit]).foreach { u =>
                      world.orderQueue.queue_!(new AttackMove(u, choke._1.center.randomized(3)))
                    }
                  }
              }
          }
        case Nil =>

      }) match {
        case Success(_) =>
        case Failure(ex) =>
          ex.printStackTrace()
      }
    }
  })

  override def lazyWorld: DefaultWorld = main.world
  override def universe: Universe = main.universe
  override protected def tickPlugIn(): Unit = {
    // nop
  }

  private def debugUnit(wrapsUnit: UnitWithJob[_ <: WrapsUnit]): Unit = {
    info(s"user requested inspection of $wrapsUnit")
  }
}

class MainAI extends AIPlugIn with HasUniverse with AIAPIEventDispatcher {
  lazy val brain = new TwilightSparkle(lazyWorld)

  override def debugger = world.debugger

  override def universe = brain.universe

  override protected def tickPlugIn(): Unit = {
    brain.queueOrdersForTick()
  }
}