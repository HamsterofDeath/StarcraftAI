package pony

import scala.collection.mutable

trait TechTree {
  lazy val requiredBy = {
    val result = multiMap[Class[_ <: WrapsUnit], Class[_ <: WrapsUnit]]
    dependsOn.foreach { case (target, needs) =>
      needs.foreach { c =>
        result.addBinding(c, target)
      }
    }
    result.toImmutable
  }
  protected val builtBy  : Map[Class[_ <: WrapsUnit], Class[_ <: WrapsUnit]]
  protected val dependsOn: Map[Class[_ <: WrapsUnit], Set[_ <: Class[_ <: Building]]]
  protected val upgrades : Map[Upgrade, Class[_ <: Upgrader]]
  private val requirementsCache = mutable.HashMap
                                  .empty[Class[_ <: WrapsUnit], Set[Class[_ <: Building]]]
  def mainBuildingOf(addon: Class[_ <: Addon]) = {
    builtBy(addon).asInstanceOf[Class[_ <: CanBuildAddons]]
  }
  def upgraderFor(upgrade: Upgrade) = upgrades(upgrade)

  def canBuild(factory: Class[_ <: UnitFactory], mobile: Class[_ <: Mobile]) = {
    builtBy(mobile) == factory
  }
  def canUpgrade(u: Class[_ <: Upgrader], up: Upgrade) = upgrades(up) == u
  def canBuildAddon(main: Class[_ <: CanBuildAddons], addon: Class[_ <: Addon]) = {
    builtBy(addon) == main
  }
  def requiredFor[T <: WrapsUnit](what: Class[_ <: T]) = {
    requirementsCache.getOrElseUpdate(what, {
      val all = mutable.Set.empty[Class[_ <: Building]]
      var head = mutable.Set.empty[Class[_ <: Building]] ++= dependsOn.getOrElse(what, Set.empty)
      var goOn = true
      do {
        all ++= head
        head = head.flatMap(e => dependsOn.getOrElse(e, Set.empty))
      }
      while (head.nonEmpty)
      all.toSet
    })
  }
}

class TerranTechTree extends TechTree {

  override protected val upgrades: Map[Upgrade, Class[_ <: Upgrader]] = Map(
    Upgrades.Terran.GoliathRange -> classOf[MachineShop],
    Upgrades.Terran.CruiserGun -> classOf[PhysicsLab],
    Upgrades.Terran.CruiserEnergy -> classOf[PhysicsLab],
    Upgrades.Terran.Irradiate -> classOf[ScienceFacility],
    Upgrades.Terran.EMP -> classOf[ScienceFacility],
    Upgrades.Terran.ScienceVesselEnergy -> classOf[ScienceFacility],
    Upgrades.Terran.GhostCloak -> classOf[CovertOps],
    Upgrades.Terran.GhostEnergy -> classOf[CovertOps],
    Upgrades.Terran.GhostStop -> classOf[CovertOps],
    Upgrades.Terran.GhostVisiblityRange -> classOf[CovertOps],
    Upgrades.Terran.TankSiegeMode -> classOf[MachineShop],
    Upgrades.Terran.VultureSpeed -> classOf[MachineShop],
    Upgrades.Terran.SpiderMines -> classOf[MachineShop],
    Upgrades.Terran.WraithCloak -> classOf[ControlTower],
    Upgrades.Terran.WraithEnergy -> classOf[ControlTower],
    Upgrades.Terran.MarineRange -> classOf[Academy],
    Upgrades.Terran.MedicEnergy -> classOf[Academy],
    Upgrades.Terran.MedicHeal -> classOf[Academy],
    Upgrades.Terran.MedicFlare -> classOf[Academy],
    Upgrades.Terran.InfantryCooldown -> classOf[Academy],
    Upgrades.Terran.InfantryArmor -> classOf[EngineeringBay],
    Upgrades.Terran.InfantryWeapons -> classOf[EngineeringBay],
    Upgrades.Terran.VehicleWeapons -> classOf[Armory],
    Upgrades.Terran.VehicleArmor -> classOf[Armory],
    Upgrades.Terran.ShipArmor -> classOf[Armory],
    Upgrades.Terran.ShipWeapons -> classOf[Armory]

  )

  override protected val builtBy: Map[Class[_ <: WrapsUnit], Class[_ <: Building]] = Map(
    classOf[Comsat] -> classOf[CommandCenter],
    classOf[NuclearSilo] -> classOf[CommandCenter],
    classOf[Dropship] -> classOf[Starport],
    classOf[MachineShop] -> classOf[Factory],
    classOf[ControlTower] -> classOf[Starport],
    classOf[PhysicsLab] -> classOf[ScienceFacility],
    classOf[CovertOps] -> classOf[ScienceFacility],
    classOf[SCV] -> classOf[CommandCenter],
    classOf[Marine] -> classOf[Barracks],
    classOf[Firebat] -> classOf[Barracks],
    classOf[Ghost] -> classOf[Barracks],
    classOf[Medic] -> classOf[Barracks],
    classOf[Vulture] -> classOf[Factory],
    classOf[Tank] -> classOf[Factory],
    classOf[Goliath] -> classOf[Factory],
    classOf[Wraith] -> classOf[Starport],
    classOf[Battlecruiser] -> classOf[Starport],
    classOf[ScienceVessel] -> classOf[Starport]
  )

  override protected val dependsOn: Map[Class[_ <: WrapsUnit], Set[_ <: Class[_ <: Building]]] = {
    val inferred = builtBy.map { case (k, v) => k -> Set(v) }.toList

    val additional: Map[Class[_ <: WrapsUnit], Set[_ <: Class[_ <: Building]]] = Map(
      classOf[Factory] -> classOf[Barracks].toSet,
      classOf[Comsat] -> Set(classOf[CommandCenter], classOf[Academy]),
      classOf[Starport] -> classOf[Factory].toSet,
      classOf[ScienceFacility] -> classOf[Starport].toSet,
      classOf[ControlTower] -> classOf[Starport].toSet,
      classOf[MissileTurret] -> classOf[EngineeringBay].toSet,
      classOf[Ghost] -> classOf[CovertOps].toSet,
      classOf[Bunker] -> classOf[Barracks].toSet,
      classOf[Firebat] -> classOf[Academy].toSet,
      classOf[Medic] -> classOf[Academy].toSet,
      classOf[Tank] -> classOf[MachineShop].toSet,
      classOf[Goliath] -> Set(classOf[MachineShop], classOf[Armory]),
      classOf[Battlecruiser] -> Set(classOf[ControlTower], classOf[PhysicsLab]),
      classOf[ScienceVessel] -> Set(classOf[ScienceFacility], classOf[ControlTower]),
      classOf[Valkery] -> Set(classOf[ControlTower], classOf[Armory])
    )

    (inferred ++ additional.toList).groupBy(_._1)
    .map { case (k, vs) => k -> vs.flatMap(_._2).toSet }

  }
}

sealed trait SCRace {
  val techTree: TechTree
  def specialize[T](unitType: Class[_ <: T]) = {
    (if (classOf[WorkerUnit].isAssignableFrom(unitType)) {
      workerClass
    } else if (classOf[ResourceGatherPoint].isAssignableFrom(unitType)) {
      resourceDepositClass
    } else if (classOf[MainBuilding].isAssignableFrom(unitType)) {
      resourceDepositClass
    } else if (classOf[SupplyProvider].isAssignableFrom(unitType)) {
      supplyClass
    } else if (classOf[TransporterUnit].isAssignableFrom(unitType)) {
      transporterClass
    } else
      unitType).asInstanceOf[Class[T]]
  }
  def resourceDepositClass: Class[_ <: MainBuilding]
  def workerClass: Class[_ <: WorkerUnit]
  def transporterClass: Class[_ <: TransporterUnit]
  def supplyClass: Class[_ <: SupplyProvider]
  def detectorBuildingClass: Class[_ <: DetectorBuilding]
}

case object Terran extends SCRace {
  override val techTree = new TerranTechTree
  override def workerClass = classOf[SCV]
  override def transporterClass = classOf[Dropship]
  override def supplyClass = classOf[SupplyDepot]
  override def resourceDepositClass = classOf[CommandCenter]
  override def detectorBuildingClass = classOf[MissileTurret]
}

case object Zerg extends SCRace {
  override val techTree = ???
  override def workerClass = classOf[Drone]
  override def transporterClass = classOf[Overlord]
  override def supplyClass = classOf[Overlord]
  override def resourceDepositClass = classOf[Hive]
  override def detectorBuildingClass = classOf[SporeColony]
}

case object Protoss extends SCRace {
  override val techTree = ???
  override def workerClass = classOf[Probe]
  override def transporterClass = classOf[Shuttle]
  override def supplyClass = classOf[Pylon]
  override def resourceDepositClass = classOf[Nexus]
  override def detectorBuildingClass = classOf[PhotonCannon]
}

case object Other extends SCRace {
  override val techTree = ???
  override def workerClass = ???
  override def transporterClass = ???
  override def supplyClass = ???
  override def resourceDepositClass = ???
  override def detectorBuildingClass = ???
}