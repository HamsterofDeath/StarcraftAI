package pony
package brain
package modules
package strategy

import pony.brain.modules.micro.WatchForCloak
import pony.units.{Academy, Comsat, ControlTower, Factory, MachineShop, Starport, TransporterUnit}

/** Addons, anti-cloak timing and the classic richest-reachable-field expansion choice shared by Terran strategies. */
trait TerranDefaults extends LongTermStrategy {

  /** A hidden enemy has been seen or has hurt us (WatchForCloak). */
  protected def cloakThreat = universe.pluginByType[WatchForCloak].threat

  /**
    * Turrets once a hidden enemy shows, else from the middle of the game; against Protoss from minute five. Dark
    * templar came from six and a half minutes on, before the first turret, and killed at our wall and in the field.
    */
  override def buildAntiCloakNow = phase.isSinceMid || cloakThreat || universe.forces.isTvP && phase.isSinceEarlyMid

  override def suggestAddons: Seq[AddonToAdd] = {
    // the scanner (and the academy it needs) once a hidden enemy shows, and against Protoss from minute four
    AddonToAdd(classOf[Comsat], requestNewBuildings = true)(
      unitManager.existsAndDone(classOf[Academy]) || phase.isSinceEarlyMid || cloakThreat ||
        universe.forces.isTvP && phase.isSinceVeryEarlyMid
    ) ::
      AddonToAdd(classOf[MachineShop], requestNewBuildings = false)(
        unitManager.existsAndDone(classOf[Factory])
      ) ::
      AddonToAdd(classOf[ControlTower], requestNewBuildings = false)(
        unitManager.existsAndDone(classOf[Starport])
      ) ::
      Nil
  }

  override def suggestNextExpansion = {
    val shouldExpand = expandNow
    if (shouldExpand) {
      val covered   = bases.allBases.flatMap(_.resourceArea).toSet
      val dangerous = mapLayers.slightlyDangerousAsBlocked
      bases.mainBase.map(_.mainBuilding.tilePosition).flatMap { where =>
        val others = {
          strategicMap.resources
            .filterNot(covered)
            .filterNot(ra => dangerous.blocked(ra.nearbyFreeTile))
            .filter { where =>
              universe.ownUnits.allByType[TransporterUnit].nonEmpty ||
              mapLayers.rawWalkableMap
                .areInSameWalkableArea(
                  where.nearbyFreeTile,
                  bases.mainBase.get.mainBuilding.tilePosition
                )
            }
        }
        if (others.nonEmpty) {
          val target = others.maxBy(e =>
            (
              e.mineralsAndGas,
              -e.patches.map(_.center.distanceSquaredTo(where))
                .getOrElse(999999)
            )
          )
          Some(target)
        } else {
          None
        }
      }
    } else {
      None
    }
  }

  protected def expandNow = {
    val (poor, rich) = bases.myMineralFields
      .partition(_.remainingPercentage < expansionThreshold)
    poor.size >= rich.size && rich.size <= 2
  }

  protected def expansionThreshold = 0.5
}
