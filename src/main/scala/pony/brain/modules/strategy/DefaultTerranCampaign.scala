package pony
package brain
package modules
package strategy

import pony.brain.budget.ResourceRequests
import pony.brain.modules.campaign.TerranCampaignConfig
import pony.brain.modules.production.{IdealProducerCount, IdealUnitRatio}
import pony.geometry.MapTilePosition
import pony.tech.Upgrades
import pony.units.{
  Barracks, Bunker, Dropship, Factory, Goliath, MachineShop, MainBuilding, Marine, Medic, ScienceVessel, Tank, Vulture
}

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
    // marines need no gas: a growing mineral bank turns into more barracks even while gas is scarce; at half this rate
    // game 3 vs Zerg on 050c38f banked 2000 to 4000 minerals for most of the game with two barracks and little gas
    IdealProducerCount(classOf[Barracks], (1 + scale) min 6)(true) ::
      IdealProducerCount(classOf[Factory], ((bases.finishedBases.size max 1) + scale) min 6)(true) :: Nil
  }

  override def suggestUnits = {
    val scale = spendScale
    IdealUnitRatio(classOf[Marine], 4 + scale * 2)(true) ::
      IdealUnitRatio(classOf[Vulture], 4 + scale * 2)(true) ::
      IdealUnitRatio(classOf[Tank], 6 + scale * 3)(true) ::
      IdealUnitRatio(classOf[Goliath], 2 + scale)(true) ::
      // medics blind the enemy army with Optical Flare (BlindEnemies) once the second base pays for them
      IdealUnitRatio(classOf[Medic], 2 + scale)(bases.finishedBases.size >= 2) ::
      // science vessels march with the army and reveal what the scanner cannot reach (EscortWithVessels), late in the
      // game once hidden enemies showed up or three bases pay for the starport, control tower and science facility
      IdealUnitRatio(classOf[ScienceVessel], 1 + scale / 4)(
        phase.isSinceLateMid && (cloakThreat || bases.finishedBases.size >= 3)
      ) ::
      // dropships carry the army to enemy bases on islands, where its ground units ask for a ferry (FerrySupport)
      IdealUnitRatio(classOf[Dropship], 1)(
        ownUnits.allByType[Dropship].size < DefaultTerranCampaign.dropships(
          islandFields,
          bases.finishedBases.size,
          ferryManager.waitingForFerry
        )
      ) :: Nil
  }

  /** Whether a field lies where no SCV can walk from our start: an island. The terrain never changes. */
  private lazy val islandFields = {
    val own    = nativeGame.self().getStartLocation
    val ground = mapLayers.rawWalkableMap
    ground.nearestFree(MapTilePosition(own.x, own.y)).exists { start =>
      strategicMap.resources.exists(a => !ground.areInSameWalkableArea(start, a.nearbyFreeTile))
    }
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(unitManager.existsAndDone(classOf[MachineShop])) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(unitManager.existsAndDone(classOf[MachineShop])) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(unitManager.existsAndDone(classOf[MachineShop])) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(bases.finishedBases.size >= 2) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(bases.finishedBases.size >= 2) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(bases.finishedBases.size >= 2) ::
      UpgradeToResearch(Upgrades.Terran.MedicFlare)(bases.finishedBases.size >= 2) ::
      // bunkered Marines step out, stim and go back in when enemies come (BunkerStimCycle)
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(ownUnits.allByType[Bunker].nonEmpty) ::
      UpgradeToResearch(Upgrades.Terran.MedicEnergy)(bases.finishedBases.size >= 3) ::
      // the vessels strip the shields and energy of Protoss armies (EmpShockwave)
      UpgradeToResearch(Upgrades.Terran.EMP)(ownUnits.allByType[ScienceVessel].nonEmpty) :: Nil
}

object DefaultTerranCampaign {

  /**
    * The dropships kept: none on a map whose fields SCVs can all walk to; else two once the third base stands or units
    * wait for a ferry, and one more for every eight units waiting, up to four. A maxed army trains nothing, so the first
    * two come before any island target is known: in game 2 vs Zerg on 369b895 on ICCup Lost_Temple 95 units at 200
    * supply stood 28 minutes beside the island Hatchery at (118,115), with 41,000 minerals banked and no dropship.
    */
  def dropships(islandFields: Boolean, finishedBases: Int, waitingForFerry: Int): Int =
    if (!islandFields || finishedBases < 3 && waitingForFerry == 0) 0
    else 2 + (waitingForFerry / 8 min 2)
}

final class DefaultTerranCampaignPlugin
    extends StrategyPlugin(
      "campaign",
      "Bunkered fields, spare home CC, then a massed mech and infantry attack",
      autoSelectable = true
    )(new DefaultTerranCampaign(_))
