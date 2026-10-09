package pony
package brain
package modules

class ProvideSuggestedAndRequestedAddons(universe: Universe)
  extends OrderlessAIModule[CanBuildAddons](universe) with AddonRequestHelper {

  override def onTick_!(): Unit = {
    val suggested = {
      val buildUs = strategy.current.suggestAddons
                    .filter(_.isActive)
      for (builder <- ownUnits.allAddonBuilders;
           addon <- buildUs
           if builder.canBuildAddon(addon.addon) & !builder.hasAddonAttached) yield (builder, addon)
    }

    suggested.filter(e => canBuildMoreOf(e._2.addon)).foreach { case (builder, what) =>
      requestAddon(what.addon, what.requestNewBuildings)
    }

    val requested = unitManager.failedToProvideByType[Addon].iterator.collect {
      case attachIt: BuildUnitRequest[Addon]
        if attachIt.proofForFunding.isSuccess &&
           universe.resources.hasStillLocked(attachIt.funding) =>
        attachIt
    }

    requested.filter { e =>
      val ok = canBuildMoreOf(e.typeOfRequestedUnit)
      if (!ok) {
        info(s"Prevented duplicate production of ${e.typeOfRequestedUnit} - fixme")
        e.forceUnlockOnDispose_!()
        e.dispose()
      }

      ok
    }.foreach { req =>
      req.clearableInNextTick_!()
      requestAddonIfResourcesProvided(req.typeOfRequestedUnit, handleDependencies = false,
        req.proofForFunding)
    }

  }

  private def canBuildMoreOf(addon: Class[? <: Addon]) = {
    val existing = ownUnits.allByClass(addon)
    if (existing.isEmpty) {
      true
    } else {
      val any = existing.head
      def noUpgrades = !any.isInstanceOf[Upgrader]
      def requiredForUnit = race.techTree
                            .requiredBy.get(any.getClass)
                            .exists(_.exists(classOf[Mobile] >= _))
      noUpgrades || requiredForUnit

    }
  }
}
