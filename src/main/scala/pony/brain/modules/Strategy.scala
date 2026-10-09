package pony
package brain
package modules

import scala.reflect.ClassTag

object Strategy {

  trait LongTermStrategy extends HasUniverse {
    val timingHelpers = new TimingHelpers

    def hasZeroOf[T <: Building: ClassTag] = ownUnits.allByType[T].isEmpty

    def name: String

    def buildAntiCloakNow: Boolean

    def suggestNextExpansion: Option[ResourceArea]
    def suggestUpgrades: Seq[UpgradeToResearch]
    def suggestUnits: Seq[IdealUnitRatio[? <: Mobile]]
    def suggestProducers: Seq[IdealProducerCount[? <: UnitFactory]]
    def suggestAddons: Seq[AddonToAdd] = Nil
    def determineScore: Int

    class TimingHelpers {

      def phase = new Phase

      class Phase {
        def isLate = isBetween(20, 9999)

        def isLateMid = isBetween(13, 20)

        def isMid = isBetween(9, 13)

        def isEarlyMid = isBetween(5, 9)

        def isBetween(from: Int, to: Int) = time.minutes >= from && time.minutes < to

        def isEarly = isBetween(0, 5)

        def isSinceVeryEarlyMid = time.minutes >= 4

        def isSinceEarlyMid = time.minutes >= 5

        def isSinceAlmostMid = time.minutes >= 7

        def isSinceMid = time.minutes >= 8

        def isSincePostMid = time.minutes >= 9

        def isSinceLateMid = time.minutes >= 13

        def isSinceVeryLateMid = time.minutes >= 16

        def isBeforeLate = time.minutes <= 20

        def isAnyTime = true
      }

    }

  }

  trait TerranDefaults extends LongTermStrategy {

    override def buildAntiCloakNow = {
      timingHelpers.phase.isSinceMid
    }

    override def suggestAddons: Seq[AddonToAdd] = {
      AddonToAdd(classOf[Comsat], requestNewBuildings = true)(
        unitManager.existsAndDone(classOf[Academy]) || timingHelpers.phase.isSinceEarlyMid
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

  case class UpgradeToResearch(upgrade: Upgrade)(active: => Boolean) {
    val maxLevel = upgrade.nativeType.fold(_.maxRepeats, _ => 1)

    def isActive = active
  }

  case class AddonToAdd(addon: Class[? <: Addon], requestNewBuildings: Boolean)(active: => Boolean) {
    def isActive = active
  }

  class Strategies(override val universe: Universe) extends HasUniverse {
    private val available = new SimpleTerran(universe) ::
      new TerranVsProtoss(universe) ::
      new TerranIsland(universe) ::
      new TerranIslandRich(universe) ::
      new TerranVsTerran(universe) ::
      new TerranVsZerg(universe) ::
      Nil
    private val byKey = Map(
      "infantry" -> new TerranInfantryPush(universe),
      "factory"  -> new TerranFactoryOnly(universe),
      "skywall"  -> new TerranSkyWall(universe),
      "carpet"   -> new TerranCarpet(universe)
    )
    private val configured = sys.props.get("twailight.strategy").filterNot(_ == "default").flatMap(byKey.get)
    private var best: LongTermStrategy = new IdleAround(universe)

    def current = configured.getOrElse(best)

    def tick(): Unit = {
      if (configured.isEmpty) ifNth(Primes.prime251) {
        best = available.maxBy(_.determineScore)
      }
    }
  }

  class SimpleTerran(override val universe: Universe) extends LongTermStrategy with TerranDefaults {
    private val config             = TerranCampaignConfig.load()
    override def name              = "Default Terran campaign"
    override def determineScore    = if (race.isTerran) 1000 else -1
    def usesBunkerDefense: Boolean = true

    /** Seal the main land choke with Supply Depots (WallWithDepots). */
    def usesWallDefense: Boolean = false

    /** The campaign may mass the army and attack the enemy base. */
    def usesCampaignLaunch: Boolean  = true
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
    private def spendScale        = ((resources.unlockedResources.minerals - 800) / 600).max(0).min(8)
    override def suggestProducers = {
      val scale = spendScale
      IdealProducerCount(classOf[Barracks], if (scale >= 4) 2 else 1)(true) ::
        IdealProducerCount(classOf[Factory], ((bases.finishedBases.size max 1) + scale) min 6)(true) :: Nil
    }
    override def suggestUnits = {
      val scale = spendScale
      IdealUnitRatio(classOf[Marine], 4 + scale * 2)(true) ::
        IdealUnitRatio(classOf[Vulture], 4 + scale * 2)(true) ::
        IdealUnitRatio(classOf[Tank], 6 + scale * 3)(true) ::
        IdealUnitRatio(classOf[Goliath], 2 + scale)(true) :: Nil
    }
    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(unitManager.existsAndDone(classOf[MachineShop])) ::
        UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(bases.finishedBases.size >= 2) ::
        UpgradeToResearch(Upgrades.Terran.VehicleArmor)(bases.finishedBases.size >= 2) ::
        UpgradeToResearch(Upgrades.Terran.GoliathRange)(bases.finishedBases.size >= 2) :: Nil
  }

  /** Maximum-offense infantry: barracks only, every infantry upgrade, no bunkers at home. */
  class TerranInfantryPush(override val universe: Universe) extends SimpleTerran(universe) {
    override def name              = "Infantry push"
    override def usesBunkerDefense = false
    override def suggestProducers  =
      IdealProducerCount(classOf[Barracks], 3)(true) :: Nil
    override def suggestUnits =
      IdealUnitRatio(classOf[Marine], 12)(true) ::
        IdealUnitRatio(classOf[Firebat], 3)(time.minutes >= 3) ::
        IdealUnitRatio(classOf[Medic], 3)(time.minutes >= 3) :: Nil
    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(true) ::
        UpgradeToResearch(Upgrades.Terran.InfantryArmor)(true) ::
        UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(true) ::
        UpgradeToResearch(Upgrades.Terran.MarineRange)(true) ::
        UpgradeToResearch(Upgrades.Terran.MedicEnergy)(true) ::
        UpgradeToResearch(Upgrades.Terran.MedicHeal)(true) ::
        UpgradeToResearch(Upgrades.Terran.MedicFlare)(true) :: Nil
  }

  /** No infantry at all: everything comes from the factory, with vehicle upgrades. */
  class TerranFactoryOnly(override val universe: Universe) extends SimpleTerran(universe) {
    override def name              = "Factory only"
    override def usesBunkerDefense = false
    override def suggestProducers  =
      IdealProducerCount(classOf[Factory], 4)(true) :: Nil
    override def suggestUnits =
      IdealUnitRatio(classOf[Vulture], 4)(true) ::
        IdealUnitRatio(classOf[Tank], 8)(true) ::
        IdealUnitRatio(classOf[Goliath], 4)(true) :: Nil
    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(true) ::
        UpgradeToResearch(Upgrades.Terran.SpiderMines)(true) ::
        UpgradeToResearch(Upgrades.Terran.VultureSpeed)(true) ::
        UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(true) ::
        UpgradeToResearch(Upgrades.Terran.VehicleArmor)(true) ::
        UpgradeToResearch(Upgrades.Terran.GoliathRange)(true) :: Nil
  }

  /** Seal the land approach with Supply Depots and fly straight to battlecruisers. */
  class TerranSkyWall(override val universe: Universe) extends SimpleTerran(universe) {
    override def name              = "Sky wall"
    override def usesBunkerDefense = false
    override def usesWallDefense   = true
    override def suggestProducers  =
      IdealProducerCount(classOf[Barracks], 1)(true) ::
        IdealProducerCount(classOf[Factory], 1)(true) ::
        IdealProducerCount(classOf[Starport], 2)(true) :: Nil
    override def suggestUnits =
      IdealUnitRatio(classOf[Battlecruiser], 8)(true) ::
        IdealUnitRatio(classOf[ScienceVessel], 1)(true) :: Nil
    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.ShipWeapons)(true) ::
        UpgradeToResearch(Upgrades.Terran.ShipArmor)(true) ::
        UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(true) ::
        UpgradeToResearch(Upgrades.Terran.CruiserGun)(true) ::
        UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(true) ::
        UpgradeToResearch(Upgrades.Terran.Irradiate)(true) :: Nil
  }

  /** Depot wall, tanks behind it, then flying factories and a mine-backed tank carpet. */
  class TerranCarpet(override val universe: Universe) extends SimpleTerran(universe) {
    override def name       = "Carpet"
    private def wallRefused = universe.pluginByType[WallWithDepots].refused
    // An unsealable choke falls back to the default bunker defense so raids find no naked base.
    override def usesBunkerDefense  = wallRefused
    override def usesWallDefense    = true
    override def usesCampaignLaunch = false
    private def spendScale          = ((resources.unlockedResources.minerals - 800) / 600).max(0).min(8)
    override def suggestProducers   = {
      val scale = spendScale
      IdealProducerCount(classOf[Barracks], if (wallRefused) 1 else 0)(true) ::
        IdealProducerCount(classOf[Factory], ((bases.finishedBases.size max 1) + scale) min 6)(true) :: Nil
    }
    override def suggestUnits = {
      val scale = spendScale
      IdealUnitRatio(classOf[Marine], if (wallRefused) 8 else 0)(true) ::
        IdealUnitRatio(classOf[Tank], 8 + scale * 3)(true) ::
        IdealUnitRatio(classOf[Vulture], 6 + scale * 2)(true) ::
        IdealUnitRatio(classOf[Goliath], 2 + scale)(true) :: Nil
    }
    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(true) ::
        UpgradeToResearch(Upgrades.Terran.SpiderMines)(true) ::
        UpgradeToResearch(Upgrades.Terran.VultureSpeed)(true) ::
        UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(true) ::
        UpgradeToResearch(Upgrades.Terran.VehicleArmor)(true) ::
        UpgradeToResearch(Upgrades.Terran.GoliathRange)(true) :: Nil
  }

  class IdleAround(override val universe: Universe) extends LongTermStrategy {

    override def name = "Idle"

    override def buildAntiCloakNow = false

    override def determineScore = -1

    override def suggestProducers = Nil

    override def suggestUnits = Nil

    override def suggestUpgrades = Nil

    override def suggestNextExpansion = None
  }

  class TerranVsProtoss(override val universe: Universe)
      extends LongTermStrategy with TerranDefaults {
    override def determineScore = {
      if (forces.isTvP) 50 else 0
    }

    override def name = "TvP"

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.remainingPercentage > 0.25)

      IdealProducerCount(classOf[Barracks], (myBases / 2) max 1)(timingHelpers.phase.isAnyTime) ::
        IdealProducerCount(classOf[Factory], 2 + myBases)(timingHelpers.phase.isAnyTime) ::
        IdealProducerCount(classOf[Starport], (myBases / 2) max 1)(
          timingHelpers.phase.isSincePostMid
        ) ::
        Nil
    }

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Medic], 1)(timingHelpers.phase.isSinceLateMid) ::
        IdealUnitRatio(classOf[Ghost], 2)(timingHelpers.phase.isSinceLateMid) ::
        IdealUnitRatio(classOf[Vulture], 6)(timingHelpers.phase.isAnyTime) ::
        IdealUnitRatio(classOf[Tank], 10)(timingHelpers.phase.isSinceEarlyMid) ::
        IdealUnitRatio(classOf[Goliath], 6)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[ScienceVessel], 2)(timingHelpers.phase.isSinceLateMid) ::
        IdealUnitRatio(classOf[Dropship], 1)(timingHelpers.phase.isSincePostMid) ::
        IdealUnitRatio(classOf[Wraith], 2)(timingHelpers.phase.isSinceLateMid) ::
        Nil
    }

    override def suggestUpgrades: Seq[UpgradeToResearch] =
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(timingHelpers.phase.isAnyTime) ::
        UpgradeToResearch(Upgrades.Terran.VultureSpeed)(timingHelpers.phase.isSinceEarlyMid) ::
        UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(timingHelpers.phase.isSinceAlmostMid) ::
        UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(timingHelpers.phase.isSinceMid) ::
        UpgradeToResearch(Upgrades.Terran.VehicleArmor)(timingHelpers.phase.isSinceMid) ::
        UpgradeToResearch(Upgrades.Terran.GoliathRange)(timingHelpers.phase.isSinceLateMid) ::
        UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isSinceLateMid) ::
        UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(
          timingHelpers.phase.isSinceLateMid
        ) ::
        UpgradeToResearch(Upgrades.Terran.Irradiate)(timingHelpers.phase.isSinceLateMid) ::
        UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.MarineRange)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(
          timingHelpers.phase.isSinceVeryLateMid
        ) ::
        UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(
          timingHelpers.phase.isSinceVeryLateMid
        ) ::
        UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.GhostCloak)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.GhostEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(
          timingHelpers.phase.isSinceVeryLateMid
        ) ::
        Nil
  }

  class TerranIslandRich(override val universe: Universe)
      extends TerranIsland(universe) with TerranDefaults {

    override def name = "Heavy air"

    override def suggestUnits: List[IdealUnitRatio[? <: Mobile]] = {
      IdealUnitRatio(classOf[ScienceVessel], 3)(timingHelpers.phase.isAnyTime) ::
        IdealUnitRatio(classOf[Battlecruiser], 10)(timingHelpers.phase.isAnyTime) ::
        super.suggestUnits.toList
    }

    override def determineScore: Int = {
      super.determineScore + bases.rich.ifElse(10, -10)
    }

    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isAnyTime) ::
        UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isAnyTime) ::
        UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isAnyTime) ::
        UpgradeToResearch(Upgrades.Terran.ShipWeapons)(timingHelpers.phase.isAnyTime) ::
        UpgradeToResearch(Upgrades.Terran.ShipArmor)(timingHelpers.phase.isAnyTime) ::
        super.suggestUpgrades.toList

  }

  class TerranVsTerran(override val universe: Universe)
      extends LongTermStrategy with TerranDefaults {
    override def name = "TvT"

    override def determineScore = {
      val ok = forces.isTvT
      if (ok) 50 else 0
    }

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Medic], 1)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Ghost], 3)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Vulture], 3)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Tank], 3)(timingHelpers.phase.isSincePostMid) ::
        IdealUnitRatio(classOf[Dropship], 3)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Goliath], 5)(timingHelpers.phase.isSincePostMid) ::
        IdealUnitRatio(classOf[Wraith], 20)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[ScienceVessel], 5)(timingHelpers.phase.isSincePostMid) ::
        Nil
    }

    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.WraithCloak)(timingHelpers.phase.isSinceEarlyMid) ::
        UpgradeToResearch(Upgrades.Terran.WraithEnergy)(timingHelpers.phase.isSinceMid) ::
        Nil

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.value > 1000)

      IdealProducerCount(classOf[Barracks], myBases)(timingHelpers.phase.isAnyTime) ::
        IdealProducerCount(classOf[Factory], myBases)(timingHelpers.phase.isAnyTime) ::
        IdealProducerCount(classOf[Starport], myBases * 3)(timingHelpers.phase.isAnyTime) ::
        Nil
    }
  }

  class TerranIsland(override val universe: Universe)
      extends LongTermStrategy with TerranDefaults {

    override def name = "Air control"

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Medic], 1)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Ghost], 1)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Vulture], 1)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Tank], 1)(timingHelpers.phase.isSincePostMid) ::
        IdealUnitRatio(classOf[Dropship], 1)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Goliath], 1)(timingHelpers.phase.isSincePostMid) ::
        IdealUnitRatio(classOf[Wraith], 8)(timingHelpers.phase.isSinceMid) ::
        IdealUnitRatio(classOf[Battlecruiser], 3)(timingHelpers.phase.isSinceLateMid) ::
        IdealUnitRatio(classOf[ScienceVessel], 2)(timingHelpers.phase.isSincePostMid) ::
        Nil
    }

    override def determineScore: Int = {
      bases.mainBase.map { mb =>
        mapLayers.isOnIsland(mb.mainBuilding.tilePosition)
          .ifElse(100, 0)
      }.getOrElse(0)
    }

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.value > 1000)

      IdealProducerCount(classOf[Barracks], myBases)(timingHelpers.phase.isAnyTime) ::
        IdealProducerCount(classOf[Factory], myBases)(timingHelpers.phase.isAnyTime) ::
        IdealProducerCount(classOf[Starport], myBases * 3)(timingHelpers.phase.isAnyTime) ::
        Nil
    }

    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.WraithCloak)(timingHelpers.phase.isAnyTime) ::
        UpgradeToResearch(Upgrades.Terran.ShipWeapons)(timingHelpers.phase.isAnyTime) ::
        UpgradeToResearch(Upgrades.Terran.WraithEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.ShipArmor)(timingHelpers.phase.isAnyTime) ::
        UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.SpiderMines)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(
          timingHelpers.phase.isSinceVeryLateMid
        ) ::
        UpgradeToResearch(Upgrades.Terran.GhostCloak)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.Irradiate)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.VehicleArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(
          timingHelpers.phase.isSinceVeryLateMid
        ) ::
        UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(
          timingHelpers.phase.isSinceVeryLateMid
        ) ::
        UpgradeToResearch(Upgrades.Terran.GoliathRange)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.VultureSpeed)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.MedicFlare)(timingHelpers.phase.isSinceMid) ::
        UpgradeToResearch(Upgrades.Terran.MedicHeal)(timingHelpers.phase.isSinceMid) ::
        UpgradeToResearch(Upgrades.Terran.MedicEnergy)(timingHelpers.phase.isSinceMid) ::
        UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.GhostEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
        UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(
          timingHelpers.phase.isSinceVeryLateMid
        ) ::
        Nil
  }

  class TerranVsZerg(override val universe: Universe)
      extends LongTermStrategy with TerranDefaults {

    override def name = "M&Ms"

    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(
        timingHelpers.phase.isSinceVeryEarlyMid
      ) ::
        UpgradeToResearch(Upgrades.Terran.MarineRange)(timingHelpers.phase.isSinceEarlyMid) ::
        UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(timingHelpers.phase.isSinceMid) ::
        UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isSinceMid) ::
        UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(timingHelpers.phase.isSinceMid) ::
        Nil

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 15)(timingHelpers.phase.isAnyTime) ::
        IdealUnitRatio(classOf[Firebat], 3)(timingHelpers.phase.isSinceEarlyMid) ::
        IdealUnitRatio(classOf[Medic], 4)(timingHelpers.phase.isSinceEarlyMid) ::
        IdealUnitRatio(classOf[Vulture], 1)(timingHelpers.phase.isSinceLateMid) ::
        IdealUnitRatio(classOf[Tank], 3)(timingHelpers.phase.isSinceAlmostMid) ::
        IdealUnitRatio(classOf[Goliath], 3)(timingHelpers.phase.isSinceLateMid) ::
        IdealUnitRatio(classOf[Wraith], 1)(timingHelpers.phase.isSinceLateMid) ::
        IdealUnitRatio(classOf[Battlecruiser], 1)(timingHelpers.phase.isLate) ::
        IdealUnitRatio(classOf[ScienceVessel], 1)(timingHelpers.phase.isSinceLateMid) ::
        Nil
    }

    override def determineScore = {
      if (forces.isTvZ || mapLayers.rawWalkableMap.size <= 96 * 96) 50 else 0
    }

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.value > 1000)
      IdealProducerCount(classOf[Barracks], myBases + 2)(timingHelpers.phase.isAnyTime) ::
        IdealProducerCount(classOf[Barracks], (myBases / 3) max 1)(
          timingHelpers.phase.isSincePostMid
        ) ::
        IdealProducerCount(classOf[Factory], myBases)(
          timingHelpers.phase.isSinceAlmostMid,
          hasZeroOf[Factory]
        ) ::
        IdealProducerCount(classOf[Starport], (myBases / 2) max 1)(
          timingHelpers.phase.isSinceLateMid,
          hasZeroOf[Starport]
        ) ::
        Nil
    }
  }

}
