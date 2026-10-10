package pony
package brain
package modules
package strategy

/**
  * The main stays sealed behind its ramp wall for the whole game. Dropships carry SCVs to the outer fields and back,
  * a few Marines and two sieged tanks hold the ramp, and Battlecruisers raid the enemy, flying home to a repair crew
  * whenever they are hurt. Money left over buys Vultures and spider mines.
  */
class TerranSealedCruisers(universe: Universe) extends DefaultTerranCampaign(universe) {
  override def name = "Sealed cruisers"

  // marines cannot reach bunkers outside the wall; the cruisers defend the outer fields
  override def usesBunkerDefense = false

  override def usesWallDefense = true

  override def usesCampaignLaunch = false

  override def sealsMain = true

  override def raidsWithCruisers = true

  // a cruiser takes six supply: game 82 sat at 191 supply with 125 SCVs and room for ten cruisers
  override def maxWorkers = 60

  // cruisers live on gas and every field has one geyser: idle minerals buy fields
  override def extraFieldsForGas = 3

  private def fewer[T <: WrapsUnit: scala.reflect.ClassTag](than: Int) = ownUnits.allByType[T].size < than

  override def suggestProducers = {
    val scale = spendScale
    IdealProducerCount(classOf[Barracks], 1)(true) ::
      IdealProducerCount(classOf[Factory], 1)(true) ::
      IdealProducerCount(classOf[Starport], (bases.finishedBases.size max 1) + 1 + scale / 2 min 6)(true) :: Nil
  }

  override def suggestAddons =
    super.suggestAddons :+
      AddonToAdd(classOf[PhysicsLab], requestNewBuildings = true)(unitManager.existsAndDone(classOf[Starport]))

  // ratios rule production; a ground or transport type only counts while it is below its cap
  override def suggestUnits = {
    val scale = spendScale
    IdealUnitRatio(classOf[Marine], 6)(fewer[Marine](6)) ::
      IdealUnitRatio(classOf[Tank], 2)(fewer[Tank](2)) ::
      IdealUnitRatio(classOf[Dropship], 3)(fewer[Dropship](3) && unitManager.existsAndDone(classOf[Starport])) ::
      // a repair costs less than a new cruiser: while hurt ones wait, a low gas bank goes to the crew
      IdealUnitRatio(classOf[Battlecruiser], 12 + scale * 2)(
        worldDominationPlan.cruisersInRepair == 0 || resources.unlockedResources.gas >= 450
      ) ::
      IdealUnitRatio(classOf[Vulture], scale)(scale > 0 && fewer[Vulture](8)) ::
      // detectors for the fleet: cloaked units are hunted with the cruisers, not by guessing comsat sweeps
      IdealUnitRatio(classOf[ScienceVessel], 2)(
        fewer[ScienceVessel](2) && unitManager.existsAndDone(classOf[ScienceFacility])
      ) :: Nil
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(unitManager.existsAndDone(classOf[MachineShop])) ::
      UpgradeToResearch(Upgrades.Terran.ShipWeapons)(unitManager.existsAndDone(classOf[Battlecruiser])) ::
      UpgradeToResearch(Upgrades.Terran.ShipArmor)(unitManager.existsAndDone(classOf[Battlecruiser])) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(spendScale > 0) ::
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(unitManager.existsAndDone(classOf[Battlecruiser])) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(ownUnits.allByType[Battlecruiser].size >= 6) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(ownUnits.allByType[ScienceVessel].nonEmpty) :: Nil
}

final class TerranSealedCruisersPlugin
    extends StrategyPlugin(
      "cruisers",
      "Sealed ramp wall, dropships ferry SCVs, Battlecruisers raid and come home for repair",
      autoSelectable = false
    )(new TerranSealedCruisers(_))
