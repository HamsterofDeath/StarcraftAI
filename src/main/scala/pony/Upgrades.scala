package pony

import bwapi._

object Upgrades {

  def allTech = Terran.allTech

  trait CastOnOrganic extends SingleTargetMagicSpell {
    override val canCastOn = classOf[Organic]
  }

  trait CastOnMechanic extends SingleTargetMagicSpell {
    override val canCastOn = classOf[Mechanic]
  }

  trait CastOnAll extends SingleTargetMagicSpell {
    override val canCastOn = classOf[Mobile]
  }

  trait CastAtFreeTile extends SinglePointMagicSpell {
    override def canBeCastAt(where: MapTilePosition, by: Mobile) =
      by.universe.mapLayers.freeWalkableTiles.free(where)
  }

  trait CastOnSelf extends SingleTargetMagicSpell {
    self =>
  }

  trait SingleTargetMagicSpell extends Upgrade with IsTech {
    val canCastOn: Class[? <: Mobile]

    def asNativeTech = nativeType match {
      case Right(tt) => tt
      case _         => !!!
    }

    def asNativeUpgrade = nativeType match {
      case Left(ut) => ut
      case _        => !!!
    }

    def energyNeeded = energyCost

  }

  trait SinglePointMagicSpell extends Upgrade with IsTech {
    def asNativeTech = nativeType match {
      case Right(tt) => tt
      case _         => !!!
    }

    def asNativeUpgrade = nativeType match {
      case Left(ut) => ut
      case _        => !!!
    }

    def energyNeeded = energyCost

    def canBeCastAt(where: MapTilePosition, by: Mobile): Boolean
  }

  trait PermanentSpell extends Upgrade

  trait IsTech extends Upgrade {
    // calling this is an assertion
    nativeTech

    def nativeTech = nativeType match {
      case Left(_)  => !!!
      case Right(t) => t
    }

    def priorityRule: Option[Mobile => Double] = None
  }

  object Protoss {

    case object InfantryArmor extends Upgrade(UpgradeType.None)

    case object VehicleArmor extends Upgrade(UpgradeType.None)

    case object ShipArmor extends Upgrade(UpgradeType.None)

  }

  object Zerg {

    case object InfantryArmor extends Upgrade(UpgradeType.None)

    case object VehicleArmor extends Upgrade(UpgradeType.None)

    case object ShipArmor extends Upgrade(UpgradeType.None)

  }

  object Fake {

    case object BuildingArmor extends Upgrade(UpgradeType.None)

  }

  object Terran {

    val allTech = InfantryCooldown :: MedicFlare :: MedicHeal :: SpiderMines :: Defensematrix ::
      TankSiegeMode :: EMP :: Irradiate :: GhostStop :: GhostCloak :: WraithCloak ::
      CruiserGun :: Nil

    case object WraithEnergy extends Upgrade(UpgradeType.Apollo_Reactor)

    case object ShipArmor extends Upgrade(UpgradeType.Terran_Ship_Plating)

    case object VehicleArmor extends Upgrade(UpgradeType.Terran_Vehicle_Plating)

    case object InfantryArmor extends Upgrade(UpgradeType.Terran_Infantry_Armor)

    case object InfantryCooldown
        extends Upgrade(TechType.Stim_Packs) with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanUseStimpack]
    }

    case object InfantryWeapons extends Upgrade(UpgradeType.Terran_Infantry_Weapons)

    case object VehicleWeapons extends Upgrade(UpgradeType.Terran_Vehicle_Weapons)

    case object ShipWeapons extends Upgrade(UpgradeType.Terran_Ship_Weapons)

    case object MarineRange extends Upgrade(UpgradeType.U_238_Shells)

    case object MedicEnergy extends Upgrade(UpgradeType.Caduceus_Reactor)

    case object MedicFlare
        extends Upgrade(TechType.Optical_Flare) with SingleTargetMagicSpell with CastOnAll with DetectorsFirst

    case object MedicHeal
        extends Upgrade(TechType.Restoration) with SingleTargetMagicSpell with CastOnAll

    case object GoliathRange extends Upgrade(UpgradeType.Charon_Boosters)

    case object SpiderMines
        extends Upgrade(TechType.Spider_Mines) with SinglePointMagicSpell with CastAtFreeTile

    case object ScannerSweep
        extends Upgrade(TechType.Scanner_Sweep) with SinglePointMagicSpell with CastAtFreeTile

    case object Nuke
        extends Upgrade(TechType.Nuclear_Strike) with SinglePointMagicSpell with CastAtFreeTile

    case object Defensematrix
        extends Upgrade(TechType.Defensive_Matrix) with SingleTargetMagicSpell with CastOnAll

    case object VultureSpeed extends Upgrade(UpgradeType.Ion_Thrusters)

    case object TankSiegeMode
        extends Upgrade(TechType.Tank_Siege_Mode) with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanSiege]
    }

    case object EMP
        extends Upgrade(TechType.EMP_Shockwave) with SingleTargetMagicSpell with CastOnAll

    case object Irradiate
        extends Upgrade(TechType.Irradiate) with SingleTargetMagicSpell with CastOnAll

    case object ScienceVesselEnergy extends Upgrade(UpgradeType.Titan_Reactor)

    case object GhostStop
        extends Upgrade(TechType.Lockdown) with SingleTargetMagicSpell with CastOnMechanic with ByPrice

    case object GhostVisiblityRange extends Upgrade(UpgradeType.Ocular_Implants)

    case object GhostEnergy extends Upgrade(UpgradeType.Moebius_Reactor)

    case object GhostCloak
        extends Upgrade(TechType.Personnel_Cloaking) with PermanentSpell with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanCloak]
    }

    case object WraithCloak
        extends Upgrade(TechType.Cloaking_Field) with PermanentSpell with SingleTargetMagicSpell with CastOnSelf {
      override val canCastOn = classOf[CanCloak]
    }

    case object CruiserGun
        extends Upgrade(TechType.Yamato_Gun) with SingleTargetMagicSpell with CastOnAll

    case object CruiserEnergy extends Upgrade(UpgradeType.Colossus_Reactor)

  }

}
