package pony

import pony.brain.{HasUniverse, Universe}

/**
  * Moves the StarCraft screen to the most interesting place: fights first, then visible enemy forces, then the
  * largest own army group, then home. Scrolling by hand pauses it for a few seconds. It never runs headless.
  * Switch it off with -Dtwailight.autoCamera=false and tune the minimum shot length with
  * -Dtwailight.autoCameraDwellFrames.
  */
class AutoCamera(override val universe: Universe) extends AIPlugIn with HasUniverse {
  private val enabled = sys.props.get("twailight.autoCamera").forall(_ != "false") &&
                        !sys.props.get("twailight.headless").contains("true")
  private val director = new AutoCameraDirector(
    minDwellFrames = sys.props.get("twailight.autoCameraDwellFrames").flatMap(_.toIntOption).filter(_ > 0)
                     .getOrElse(96),
    interruptMargin = CameraFocus.EnemySightingScore)
  private val scanFrames       = 12
  private val manualHoldFrames = 240
  private val clusterRadius    = 8

  /** Screen moves can show up a few frames after the command; within this window a move counts as ours. */
  private val commandLagFrames = 3

  private var previousScreen   = Option.empty[(Int, Int)]
  private var lastCommandFrame = -commandLagFrames - 1
  private var stalledCommands  = 0
  private var holdUntil        = 0
  private var unreachable      = Option.empty[(Int, Int)]

  override protected def tickPlugIn(): Unit = {
    if (enabled) {
      val frame = currentTick
      val native = nativeGame.getScreenPosition
      val screen = (native.getX, native.getY)
      val moved = previousScreen.exists(_ != screen)
      if (moved && frame - lastCommandFrame > commandLagFrames) {
        if (frame >= holdUntil) NativeMatchEvidence.trace("camera-manual-hold", s"screen=$screen frames=$manualHoldFrames")
        holdUntil = frame + manualHoldFrames
      }
      if (moved) stalledCommands = 0
      else if (lastCommandFrame == frame - 1) stalledCommands += 1
      previousScreen = Some(screen)

      if (frame % scanFrames == 0) {
        val before = director.focus
        director.consider(frame, candidates).filterNot(before.contains).foreach { focus =>
          if (!before.exists(old => old.reason == focus.reason && old.tile.distanceTo(focus.tile) <= 12)) {
            NativeMatchEvidence.trace("camera-shot", s"reason=${focus.reason} tile=${focus.tile} score=${focus.score}")
          }
        }
      }

      if (frame >= holdUntil) director.focus.foreach { focus =>
        val target = CameraPan.screenFor(focus.tile, nativeGame.mapWidth(), nativeGame.mapHeight())
        if (stalledCommands > commandLagFrames) {
          // the engine keeps refusing this position, for example at a map edge it clamps differently
          NativeMatchEvidence.trace("camera-unreachable", s"target=$target screen=$screen")
          unreachable = Some(target)
          stalledCommands = 0
        }
        if (!CameraPan.arrived(screen, target) && !unreachable.contains(target)) {
          val next = CameraPan.step(screen, target)
          nativeGame.setScreenPosition(next._1, next._2)
          lastCommandFrame = frame
        }
      }
    }
  }

  private def candidates: Seq[CameraFocus] = {
    val fighting = ownUnits.allByType[ArmedMobile].filter(u => u.isInGame && u.isInFight).map(_.currentTile) ++
                   ownUnits.allBuildings.filter(b => b.isInGame && b.isBeingAttacked).map(_.centerTile)
    val combat = AutoCameraDirector.densest(fighting.toVector, clusterRadius).map { case (tile, count) =>
      CameraFocus(tile, CameraFocus.CombatScore + count * 10, "combat")
    }
    val visibleEnemies = enemies.allByType[ArmedMobile]
                         .filter(u => u.isInGame && u.nativeUnit.isVisible && !u.isInstanceOf[WorkerUnit])
                         .map(_.currentTile)
    val sighting = AutoCameraDirector.densest(visibleEnemies.toVector, clusterRadius).map { case (tile, count) =>
      CameraFocus(tile, CameraFocus.EnemySightingScore + count, "enemy forces")
    }
    val army = ownUnits.allByType[ArmedMobile].filter(u => u.isInGame && !u.isInstanceOf[WorkerUnit])
               .map(_.currentTile)
    val armyGroup = AutoCameraDirector.densest(army.toVector, clusterRadius).map { case (tile, count) =>
      CameraFocus(tile, CameraFocus.ArmyScore + count, "army")
    }
    val home = bases.mainBase.map(base => CameraFocus(base.mainBuilding.tilePosition, CameraFocus.HomeScore, "home"))
    (combat ++ sighting ++ armyGroup ++ home).toSeq
  }
}
