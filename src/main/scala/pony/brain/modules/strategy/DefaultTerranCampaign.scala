package pony
package brain
package modules
package strategy

/** The default Terran campaign: bunkered mineral fields, a spare home Command Center, then a massed army attack. */
class DefaultTerranCampaign(override val universe: Universe) extends LongTermStrategy with TerranDefaults {
  private val config = TerranCampaignConfig.load()

  override def name = "Default Terran campaign"

  override def determineScore = if (race.isTerran) 1000 else -1

  override def runsTerranCampaign = true

  override def usesBunkerDefense = true

  override def usesCampaignLaunch = true

  override protected def expandNow = {
    val cost = ResourceRequests.forUnit(race, race.resourceDepositClass)
    config.expand(
      resources.unlockedResources.minerals,
      resources.unlockedResources.gas,
      cost.minerals,
      cost.gas,
      unitManager.requestedToBuild(race.resourceDepositClass) ||
        unitManager.constructionsInProgress[MainBuilding].nonEmpty,
      safeReachableSite = true
    )
  }

  /** Grows from 0 to 8 as unspent minerals pile up beyond 800, so a full bank turns into more production. */
  protected def spendScale = ((resources.unlockedResources.minerals - 800) / 600).max(0).min(8)

  override def suggestProducers = {
    val scale = spendScale
    // marines need no gas: a growing mineral bank turns into more barracks even while gas is scarce
    IdealProducerCount(classOf[Barracks], (1 + scale / 2) min 5)(true) ::
      IdealProducerCount(classOf[Factory], ((bases.finishedBases.size max 1) + scale) min 6)(true) :: Nil
  }

  override def suggestUnits = {
    val scale = spendScale
    IdealUnitRatio(classOf[Marine], 4 + scale * 2)(true) ::
      IdealUnitRatio(classOf[Vulture], 4 + scale * 2)(true) ::
      IdealUnitRatio(classOf[Tank], 6 + scale * 3)(true) ::
      IdealUnitRatio(classOf[Goliath], 2 + scale)(true) ::
      // medics blind the enemy army with Optical Flare (BlindEnemies) once the second base pays for them
      IdealUnitRatio(classOf[Medic], 2 + scale)(bases.finishedBases.size >= 2) :: Nil
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(unitManager.existsAndDone(classOf[MachineShop])) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(bases.finishedBases.size >= 2) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(bases.finishedBases.size >= 2) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(bases.finishedBases.size >= 2) ::
      UpgradeToResearch(Upgrades.Terran.MedicFlare)(bases.finishedBases.size >= 2) ::
      // bunkered Marines step out, stim and go back in when enemies come (BunkerStimCycle)
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(ownUnits.allByType[Bunker].nonEmpty) ::
      UpgradeToResearch(Upgrades.Terran.MedicEnergy)(bases.finishedBases.size >= 3) :: Nil
}

final class DefaultTerranCampaignPlugin
    extends StrategyPlugin(
      "campaign",
      "Bunkered fields, spare home CC, then a massed mech and infantry attack",
      autoSelectable = true
    )(new DefaultTerranCampaign(_))
