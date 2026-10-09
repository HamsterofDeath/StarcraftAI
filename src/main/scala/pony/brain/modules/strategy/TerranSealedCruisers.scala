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
      IdealUnitRatio(classOf[Dropship], 2)(fewer[Dropship](2) && unitManager.existsAndDone(classOf[Starport])) ::
      IdealUnitRatio(classOf[Battlecruiser], 12 + scale * 2)(true) ::
      IdealUnitRatio(classOf[Vulture], scale)(scale > 0 && fewer[Vulture](8)) :: Nil
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(unitManager.existsAndDone(classOf[MachineShop])) ::
      UpgradeToResearch(Upgrades.Terran.ShipWeapons)(unitManager.existsAndDone(classOf[Battlecruiser])) ::
      UpgradeToResearch(Upgrades.Terran.ShipArmor)(unitManager.existsAndDone(classOf[Battlecruiser])) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(spendScale > 0) :: Nil
}

final class TerranSealedCruisersPlugin
    extends StrategyPlugin(
      "cruisers",
      "Sealed ramp wall, dropships ferry SCVs, Battlecruisers raid and come home for repair",
      autoSelectable = false
    )(new TerranSealedCruisers(_))
