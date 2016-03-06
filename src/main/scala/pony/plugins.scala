package pony

import java.text.DecimalFormat

import bwapi.Color
import pony.AttackPriorities.Highest
import pony.brain.modules.{GatherMineralsAtSinglePatch, ProvideExpansions}
import pony.brain.{HasUniverse, TwilightSparkle, Universe}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

class MapReveal extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    debugger.chat("black sheep wall")
  }
}

class FastSpeed extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    debugger.fastest()
  }
}

class ChokePointRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Green)
      strategicMap.domains.foreach { case (choke, _) =>
        choke.lines.foreach { line =>
          renderer.drawLine(line.absoluteFrom, line.absoluteTo)
          renderer.drawTextAtTile(s"Chokepoint ${choke.index}", line.center)
        }
      }

      strategicMap.narrowPoints.foreach { narrow =>
        renderer.drawStar(narrow.where, 1)
        renderer.drawTextAtTile(s"Narrow ${narrow.index}", narrow.where)
      }
    }
  }
}

class WalkableRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    if (lazyWorld.debugger.isFullDebug) {
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
}

class BuildableRenderer(ignoreWalkable: Boolean) extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    if (lazyWorld.debugger.isFullDebug) {
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
}

class UnitIdRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Green)

      lazyWorld.ownUnits.allByType[Mobile]
      .iterator
      .collect {
        case g: GroundUnit if !g.loaded => g
        case a: AirUnit => a
      }
      .foreach { u =>
        renderer.drawTextAtMobileUnit(u, u.unitIdText)
      }
      lazyWorld.ownUnits.allByType[Building].foreach { u =>
        renderer.drawTextAtStaticUnit(u, s"${u.unitIdText}/${u.getClass.className}")
      }
      lazyWorld.enemyUnits.allByType[Mobile].foreach { u =>
        renderer.drawTextAtMobileUnit(u, u.unitIdText)
      }
      lazyWorld.enemyUnits.allByType[Building].foreach { u =>
        renderer.drawTextAtStaticUnit(u, s"${u.unitIdText}/${u.getClass.className}")
      }
    }
  }
}

class UnitJobRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      val renderUs = unitManager.allJobsByUnitType[Mobile].filter { job =>
        job.unit match {
          case g: GroundUnit if !g.loaded => true
          case a: AirUnit => true
          case _ => false
        }
      }

      renderUs.foreach { job =>
        renderer.drawTextAtMobileUnit(job.unit,
          s"${job.shortDebugString} -> ${job.unit.nativeUnit.getOrder}", 1)
        job.renderDebug(renderer)
      }
      unitManager.allJobsByUnitType[Building].foreach { job =>
        renderer.drawTextAtStaticUnit(job.unit,
          s"${job.shortDebugString} -> ${job.unit.nativeUnit.getOrder}", 1)
      }
    }
  }

  override def lazyWorld: DefaultWorld = universe.world
}

class UnitSecondLevelJobRenderer(override val universe: Universe)
  extends AIPlugIn with HasUniverse {

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>

    }
  }

  override def lazyWorld: DefaultWorld = universe.world
}

class PathDebugRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Purple)
      universe.worldDominationPlan.allAttacks.foreach { attack =>
        val center = attack.currentCenter
        val count = attack.force.size
        renderer.drawCircleAround(center.asMapPosition, math.round(math.sqrt(count)).toInt)
        attack.completePath.result.foreach { path =>
          path.renderDebug(renderer)
        }
      }
    }
  }
}

class UnitDebugRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      universe.ownUnits.allCompletedMobiles.filter(_.isSelected).foreach { m =>
        val center = m.currentPosition
        m match {
          case a: AirWeapon =>
            renderer.in_!(Color.Yellow).drawCircleAround(center, a.airRangePixels)
          case _ =>

        }
        m match {
          case g: GroundWeapon =>
            renderer.in_!(Color.Red).drawCircleAround(center, g.groundRangePixels)
          case _ =>

        }
      }

      universe.enemyUnits.allCompletedMobiles.filter(_.isHarmlessNow).foreach { cd =>
        renderer.in_!(Color.Green).drawCrossedOutOnTile(cd.currentTile)
      }
    }
  }
}

class StatsRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val lazyWorld     = universe.world
  private  val df            = new DecimalFormat("#0.00")
  private  var lastTickNanos = System.nanoTime()

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      val current = System.nanoTime()
      val diff = current - lastTickNanos
      lastTickNanos = current

      val debugString = ArrayBuffer.empty[String]

      val speedFactor = {
        val secondsPerTick = diff.toDouble / 1000 / 1000 / 1000
        1 / secondsPerTick / 24
      }

      debugString += {
        val time = universe.time.formatted
        val category = universe.time.categoryName

        s"$category: $time (*${df.format(speedFactor)})"
      }

      debugString += {
        val locked = resources.lockedResources
        val forceLocked = resources.forceLocks
        val locks = resources.detailedLocks
        val allLocks = forceLocked ++ locks
        val counts = allLocks.map(_.whatFor).groupBy(identity).map { case (c, am) => c -> am.size }
        val details = counts.toList.map { case (k, v) => s"${k.className}*$v" }.mkString(", ")
        s"Plan: ${locked.minerals}m, ${locked.gas}g, ${locked.supply}s, ${allLocks.size}L, $details"
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

        val missingUnits = unitManager.failedToProvideFlat.groupBy(_.typeOfRequestedUnit)
                           .mapValues(_.size)
        val formatted = missingUnits
                        .map { case (unitClass, howMany) => s"${unitClass.className}/$howMany" }
        s"Type/missing: ${formatted.toList.sorted.mkString(", ")}"
      }

      debugString ++= {
        universe.bases.bases.flatMap { base =>
          base.myMineralGroup.map { mins =>
            val gatherJob = unitManager.allJobsByType[GatherMineralsAtSinglePatch]
                            .filter(e => mins.contains(e.targetPatch))
            val stats = resources.stats
            val minsGot = stats.mineralsPerMinute.toInt
            val minsGotPerWorker = df.format(minsGot.toDouble / gatherJob.size)
            s"Base ${base.mainBuilding.unitIdText}: ${mins.value}m, ${
              gatherJob.size
            } workers, $minsGot income ($minsGotPerWorker avg)"
          }
        }.toList.sorted
      }

      debugString ++= {
        universe.worldDominationPlan.allAttacks.map { att =>
          val id = att.uniqueId.toSome.map { id =>
            s"Attack $id with"
          }
          val force = att.force.size.toSome.map { i =>
            s" $i units"
          }
          val where = att.destination.where.toSome.map { tp =>
            s" attacking $tp"
          }
          val state = att.meetingStats.map { case (done, total) =>
            s", ${total - done} tbd"
          }

          (force :: where :: state :: Nil).flatten.mkString
        }
      }

      if (debugger.isFullDebug) {
        debugString ++= {
          val formatted = unitManager.jobsByType.map { case (jobType, members) =>
            s"${jobType.className}/${members.size}"
          }
          "Job/units:" :: formatted.toList.sorted
        }

        debugString ++= {
          unitManager.employers.map { emp =>
            s"$emp has ${unitManager.jobsOf(emp).size} units"
          }.toList.sorted
        }
      }

      debugString.zipWithIndex.foreach { case (txt, line) => renderer.drawTextOnScreen(txt, line) }
    }
  }
}

class BlockedBuildingSpotsRenderer(override val universe: Universe)
  extends AIPlugIn with HasUniverse {
  override val lazyWorld = universe.world

  override protected def tickPlugIn(): Unit = {
    if (lazyWorld.debugger.isFullDebug) {
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
}

class MineralDebugRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val lazyWorld = universe.world

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Yellow)
      world.resourceAnalyzer.groups.foreach { mpg =>
        mpg.patches.foreach { mp =>
          renderer.writeText(mp.tilePosition, s"#${mpg.patchId}")
        }
      }

      universe.unitManager.allJobsByType[GatherMineralsAtSinglePatch].groupBy(_.targetPatch)
      .foreach { case (k, v) =>
        val estimatedWorkerCount = v.head.requiredWorkers
        renderer.drawTextAtStaticUnit(v.head.targetPatch, estimatedWorkerCount.toString, 1)
      }

    }
  }
}

class DebugHelper(main: MainAI) extends AIPlugIn with HasUniverse {

  main.listen_!(new AIAPI {
    override def world: DefaultWorld = main.world

    override def onSendText(s: String): Unit = {
      super.onSendText(s)
      val words = s.split(' ').toList
      Try(words match {
        case command :: params =>
          command match {
            case "log" | "l" =>
              params match {
                case List(logLevel) =>

                  setLogLevel_!(logLevel match {
                    case "0" => LogLevels.LogOff
                    case "1" => LogLevels.LogError
                    case "2" => LogLevels.LogWarn
                    case "3" => LogLevels.LogInfo
                    case "4" => LogLevels.LogDebug
                    case "5" => LogLevels.LogTrace
                    case _ => !!!(logLevel)
                  })
              }
            case "expand" | "e" =>
              params match {
                case List(mineralsId) =>
                  val patch = world.resourceAnalyzer.groups.find(_.patchId.toString == mineralsId)
                              .get
                  main.brain.pluginByType[ProvideExpansions].forceExpand(patch)
              }

            case "speed" | "s" =>
              params match {
                case List(integer) =>
                  universe.world.debugger.speed(integer.toInt)
              }
            case "debugoff" | "doff" =>
              universe.world.debugger.off()
            case "debugon" | "don" =>
              universe.world.debugger.on()
            case "debugmoff" | "dmoff" =>
              universe.world.debugger.fullOff()
            case "debugmon" | "dmon" =>
              universe.world.debugger.fullOn()
            case "debug" | "d" =>
              params match {
                case List(id) =>
                  ownUnits.allKnownUnits.find(_.unitIdText == id).foreach(debugUnit)
                  enemies.allKnownUnits.find(_.unitIdText == id).foreach(debugUnit)
              }
            case "attack" | "a" =>
              val target = params match {
                case List(x, id) if x == "m" || x == "minerals" =>
                  world.resourceAnalyzer.groups.find(_.patchId.toString == id).map(_.center)

                case List(x, id) if x == "c" || x == "choke" =>
                  strategicMap.domains.find(_._1.index.toString == id).map(_._1.center)

                case List(x, id) if x == "n" || x == "narrow" =>
                  strategicMap.narrowPoints.find(_.index.toString == id).map(_.where)

                case List(x, id) if x == "u" || x == "unit" =>
                  enemies.allMobilesAndBuildings.find(_.unitIdText == id).map(_.centerTile)
              }
              target.foreach { where =>
                main.brain.universe.worldDominationPlan.initiateAttack(where, Highest)
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

  private def debugUnit(wrapsUnit: WrapsUnit): Unit = {
    info(s"user requested inspection of $wrapsUnit")
  }
}

class MainAI extends AIPlugIn with HasUniverse with AIAPIEventDispatcher {
  lazy val brain = new TwilightSparkle(lazyWorld)

  override def universe = brain.universe

  override protected def tickPlugIn(): Unit = {
    brain.queueOrdersForTick()
    if (debugger.isDebugging) {
      brain.plugins.foreach(_.renderDebug(debugger.renderer))
      brain.renderDebug(debugger.renderer)
    }
  }

  override def debugger = world.debugger
}