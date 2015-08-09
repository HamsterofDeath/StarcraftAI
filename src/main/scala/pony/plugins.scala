package pony

import java.text.DecimalFormat

import bwapi.Color
import pony.brain.modules.GatherMineralsAtSinglePatch
import pony.brain.{HasUniverse, TwilightSparkle, UnitWithJob, Universe}

import scala.collection.mutable.ArrayBuffer

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

class WalkableRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Red)

      lazyWorld.map.walkableGrid.all
      .filter(lazyWorld.map.walkableGrid.blocked)
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
        renderer.drawTextAtUnit(u, u.unitIdText)
      }
    }
  }
}

class UnitJobRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      unitManager.allJobsByUnit[Mobile].foreach { job =>
        renderer.drawTextAtUnit(job.unit, job.shortDebugString, 1)
      }
    }
  }
  override def lazyWorld: DefaultWorld = universe.world
}

class StatsRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val lazyWorld       = universe.world

  override protected def tickPlugIn(): Unit = {
    resources.stats
    lazyWorld.debugger.debugRender { renderer =>
      val debugString = ArrayBuffer.empty[String]

      debugString += {
        val locked = resources.lockedResources
        val locks = resources.detailledLocks
        s"${locked.minerals}m, ${locked.gas}g, ${locked.supply}s locked, ${locks.size} locks"
      }

      debugString ++= {
        val missingUnits = unitManager.failedToProvideFlat.groupBy(_.typeOfRequestedUnit).mapValues(_.size)
        missingUnits.map { case (unitClass, howMany) => s"Type ${unitClass.className} -> $howMany missing " }
      }

      val df = new DecimalFormat("#0.00")

      debugString ++= {
        universe.bases.bases.map { base =>
          val mins = base.myMineralGroup.get
          val gatherJob = unitManager.allJobsByType[GatherMineralsAtSinglePatch]
                          .filter(e => mins.contains(e.targetPatch))
          val stats = resources.stats
          val minsGot = stats.minerals
          val minsGotPerWorker = df.format(minsGot.toDouble / gatherJob.size)
          s"Base ${base.mainBuilding.unitIdText}: ${mins.value}m, ${
            gatherJob.size
          } workers, $minsGot income ($minsGotPerWorker avg)"
        }
      }

      debugString ++= {
        unitManager.employers.map { emp =>
          s"$emp has ${unitManager.jobsOf(emp).size} units"
        }
      }

      debugString ++= {
        unitManager.jobsByType.map { case (jobType, members) =>
          s"${jobType.className} => ${members.size} units"
        }
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
      area.all.filter(area.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Blue)
      val area = mapLayers.blockedByResources
      area.all.filter(area.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }

    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Grey)
      val area = mapLayers.blockedByWorkerPaths
      area.all.filter(area.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
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
      words match {
        case command :: params =>
          command match {
            case "debug" =>
              params match {
                case List(id) =>
                  unitManager.jobByUnitIdString(id).foreach {debugUnit}
              }
          }
        case Nil =>

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