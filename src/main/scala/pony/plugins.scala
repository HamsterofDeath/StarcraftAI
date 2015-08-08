package pony

import java.text.DecimalFormat

import bwapi.Color
import pony.brain.modules.GatherMineralsAtSinglePatch
import pony.brain.{HasUniverse, TwilightSparkle, Universe}

import scala.collection.mutable.ArrayBuffer

abstract class ControllingAI extends AIPlugIn {

}

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
    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Red)

      world.map.walkableGrid.all
      .filter(world.map.walkableGrid.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
  }
}

class UnitIdRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Green)

      world.units.mineByType[Mobile].foreach { u =>
        renderer.drawTextAtUnit(u, u.unitIdText)
      }
    }
  }
}

class UnitJobRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {

  override protected def tickPlugIn(): Unit = {
    world.debugger.debugRender { renderer =>
      unitManager.allJobsByUnit[Mobile].foreach { job =>
        renderer.drawTextAtUnit(job.unit, job.shortDebugString, 1)
      }
    }
  }
  override def world: DefaultWorld = universe.world
}

class StatsRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val world = universe.world
  private  val resourceHistory = ArrayBuffer.empty[MinsGas]
  private  val frameSize = 500
  override protected def tickPlugIn(): Unit = {
    resourceHistory += MinsGas(resources.gatheredMinerals, resources.gatheredGas)
    if (resourceHistory.size == frameSize + 1) {
      resourceHistory.remove(0)
    }

    world.debugger.debugRender { renderer =>
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
          val minsGot = resourceHistory.last.mins - resourceHistory.head.mins
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
  case class MinsGas(mins: Int, gas: Int)
}

class BlockedBuildingSpotsRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val world = universe.world

  override protected def tickPlugIn(): Unit = {
    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Orange)
      val area = mapLayers.blockedByBuildingTiles
      area.all.filter(area.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Blue)
      val area = mapLayers.blockedByResources
      area.all.filter(area.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }

    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Grey)
      val area = mapLayers.blockedByWorkerPaths
      area.all.filter(area.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
  }
}

class MainAI extends AIPlugIn {
  lazy val brain = new TwilightSparkle(world)

  override protected def tickPlugIn(): Unit = {
    brain.queueOrdersForTick()
  }
}