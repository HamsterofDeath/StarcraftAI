package pony

class TerranTechTree extends TechTree {

  override protected val upgrades: Map[Upgrade, Class[? <: Upgrader]] = Map(
    Upgrades.Terran.GoliathRange        -> classOf[MachineShop],
    Upgrades.Terran.CruiserGun          -> classOf[PhysicsLab],
    Upgrades.Terran.CruiserEnergy       -> classOf[PhysicsLab],
    Upgrades.Terran.Irradiate           -> classOf[ScienceFacility],
    Upgrades.Terran.EMP                 -> classOf[ScienceFacility],
    Upgrades.Terran.ScienceVesselEnergy -> classOf[ScienceFacility],
    Upgrades.Terran.GhostCloak          -> classOf[CovertOps],
    Upgrades.Terran.GhostEnergy         -> classOf[CovertOps],
    Upgrades.Terran.GhostStop           -> classOf[CovertOps],
    Upgrades.Terran.GhostVisiblityRange -> classOf[CovertOps],
    Upgrades.Terran.TankSiegeMode       -> classOf[MachineShop],
    Upgrades.Terran.VultureSpeed        -> classOf[MachineShop],
    Upgrades.Terran.SpiderMines         -> classOf[MachineShop],
    Upgrades.Terran.WraithCloak         -> classOf[ControlTower],
    Upgrades.Terran.WraithEnergy        -> classOf[ControlTower],
    Upgrades.Terran.MarineRange         -> classOf[Academy],
    Upgrades.Terran.MedicEnergy         -> classOf[Academy],
    Upgrades.Terran.MedicHeal           -> classOf[Academy],
    Upgrades.Terran.MedicFlare          -> classOf[Academy],
    Upgrades.Terran.InfantryCooldown    -> classOf[Academy],
    Upgrades.Terran.InfantryArmor       -> classOf[EngineeringBay],
    Upgrades.Terran.InfantryWeapons     -> classOf[EngineeringBay],
    Upgrades.Terran.VehicleWeapons      -> classOf[Armory],
    Upgrades.Terran.VehicleArmor        -> classOf[Armory],
    Upgrades.Terran.ShipArmor           -> classOf[Armory],
    Upgrades.Terran.ShipWeapons         -> classOf[Armory]
  )

  override protected val builtBy: Map[Class[? <: WrapsUnit], Class[? <: Building]] = Map(
    classOf[Comsat]        -> classOf[CommandCenter],
    classOf[NuclearSilo]   -> classOf[CommandCenter],
    classOf[Dropship]      -> classOf[Starport],
    classOf[MachineShop]   -> classOf[Factory],
    classOf[ControlTower]  -> classOf[Starport],
    classOf[PhysicsLab]    -> classOf[ScienceFacility],
    classOf[CovertOps]     -> classOf[ScienceFacility],
    classOf[SCV]           -> classOf[CommandCenter],
    classOf[Marine]        -> classOf[Barracks],
    classOf[Firebat]       -> classOf[Barracks],
    classOf[Ghost]         -> classOf[Barracks],
    classOf[Medic]         -> classOf[Barracks],
    classOf[Vulture]       -> classOf[Factory],
    classOf[Tank]          -> classOf[Factory],
    classOf[Goliath]       -> classOf[Factory],
    classOf[Wraith]        -> classOf[Starport],
    classOf[Battlecruiser] -> classOf[Starport],
    classOf[ScienceVessel] -> classOf[Starport]
  )

  override protected val dependsOn: Map[Class[? <: WrapsUnit], Set[? <: Class[? <: Building]]] = {
    val inferred = builtBy.map { case (k, v) => k -> Set(v) }.toList

    val additional: Map[Class[? <: WrapsUnit], Set[? <: Class[? <: Building]]] = Map(
      classOf[Factory]         -> classOf[Barracks].toSet,
      classOf[Comsat]          -> Set(classOf[CommandCenter], classOf[Academy]),
      classOf[Starport]        -> classOf[Factory].toSet,
      classOf[ScienceFacility] -> classOf[Starport].toSet,
      classOf[ControlTower]    -> classOf[Starport].toSet,
      classOf[MissileTurret]   -> classOf[EngineeringBay].toSet,
      classOf[Ghost]           -> classOf[CovertOps].toSet,
      classOf[Bunker]          -> classOf[Barracks].toSet,
      classOf[Firebat]         -> classOf[Academy].toSet,
      classOf[Medic]           -> classOf[Academy].toSet,
      classOf[Tank]            -> classOf[MachineShop].toSet,
      classOf[Goliath]         -> Set(classOf[MachineShop], classOf[Armory]),
      classOf[Battlecruiser]   -> Set(classOf[ControlTower], classOf[PhysicsLab]),
      classOf[ScienceVessel]   -> Set(classOf[ScienceFacility], classOf[ControlTower]),
      classOf[Valkery]         -> Set(classOf[ControlTower], classOf[Armory])
    )

    (inferred ++ additional.toList).groupBy(_._1)
      .map { case (k, vs) => k -> vs.flatMap(_._2).toSet }

  }
}
