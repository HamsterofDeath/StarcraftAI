package pony
package units

import bwapi.{Unit => APIUnit, _}

import scala.collection.immutable.HashMap
import scala.reflect.ClassTag

object UnitWrapper {

  private def lift[T <: WrapsUnit: ClassTag] = {
    val c           = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[? <: T]]
    val constructor = c.getConstructor(classOf[APIUnit])

    ((anyUnit: APIUnit) => constructor.newInstance(anyUnit)) -> c

  }

  private val mappingRules: Map[UnitType, (APIUnit => WrapsUnit, Class[? <: WrapsUnit])] =
    HashMap(
      UnitType.Resource_Vespene_Geyser -> lift[VespeneGeysir],
      UnitType.Terran_Supply_Depot     -> lift[SupplyDepot],
      UnitType.Terran_Command_Center   -> lift[CommandCenter],
      UnitType.Terran_Barracks         -> lift[Barracks],
      UnitType.Terran_Academy          -> lift[Academy],
      UnitType.Terran_Armory           -> lift[Armory],
      UnitType.Terran_Science_Facility -> lift[ScienceFacility],
      UnitType.Terran_Bunker           -> lift[Bunker],
      UnitType.Terran_Comsat_Station   -> lift[Comsat],
      UnitType.Terran_Covert_Ops       -> lift[CovertOps],
      UnitType.Terran_Control_Tower    -> lift[ControlTower],
      UnitType.Terran_Engineering_Bay  -> lift[EngineeringBay],
      UnitType.Terran_Factory          -> lift[Factory],
      UnitType.Terran_Machine_Shop     -> lift[MachineShop],
      UnitType.Terran_Missile_Turret   -> lift[MissileTurret],
      UnitType.Terran_Nuclear_Silo     -> lift[NuclearSilo],
      UnitType.Terran_Physics_Lab      -> lift[PhysicsLab],
      UnitType.Terran_Refinery         -> lift[Refinery],
      UnitType.Terran_Starport         -> lift[Starport],

      UnitType.Terran_SCV                   -> lift[SCV],
      UnitType.Terran_Firebat               -> lift[Firebat],
      UnitType.Terran_Marine                -> lift[Marine],
      UnitType.Terran_Medic                 -> lift[Medic],
      UnitType.Terran_Valkyrie              -> lift[Valkery],
      UnitType.Terran_Vulture               -> lift[Vulture],
      UnitType.Terran_Siege_Tank_Tank_Mode  -> lift[Tank],
      UnitType.Terran_Siege_Tank_Siege_Mode -> lift[Tank],
      UnitType.Terran_Goliath               -> lift[Goliath],
      UnitType.Terran_Wraith                -> lift[Wraith],
      UnitType.Terran_Science_Vessel        -> lift[ScienceVessel],
      UnitType.Terran_Battlecruiser         -> lift[Battlecruiser],
      UnitType.Terran_Dropship              -> lift[Dropship],
      UnitType.Terran_Ghost                 -> lift[Ghost],
      UnitType.Terran_Vulture_Spider_Mine   -> lift[SpiderMine],

      UnitType.Protoss_Probe        -> lift[Probe],
      UnitType.Protoss_Zealot       -> lift[Zealot],
      UnitType.Protoss_Dragoon      -> lift[Dragoon],
      UnitType.Protoss_Archon       -> lift[Archon],
      UnitType.Protoss_Dark_Archon  -> lift[DarkArchon],
      UnitType.Protoss_Dark_Templar -> lift[DarkTemplar],
      UnitType.Protoss_Reaver       -> lift[Reaver],
      UnitType.Protoss_High_Templar -> lift[Templar],
      UnitType.Protoss_Corsair      -> lift[Corsair],
      UnitType.Protoss_Interceptor  -> lift[Interceptor],
      UnitType.Protoss_Observer     -> lift[Observer],
      UnitType.Protoss_Scarab       -> lift[Scarab],
      UnitType.Protoss_Scout        -> lift[Scout],
      UnitType.Protoss_Arbiter      -> lift[Arbiter],

      UnitType.Protoss_Nexus                -> lift[Nexus],
      UnitType.Protoss_Arbiter_Tribunal     -> lift[ArbiterTribunal],
      UnitType.Protoss_Assimilator          -> lift[Assimilator],
      UnitType.Protoss_Gateway              -> lift[Gateway],
      UnitType.Protoss_Pylon                -> lift[Pylon],
      UnitType.Protoss_Citadel_of_Adun      -> lift[Citadel],
      UnitType.Protoss_Templar_Archives     -> lift[TemplarArchive],
      UnitType.Protoss_Shield_Battery       -> lift[ShieldBattery],
      UnitType.Protoss_Cybernetics_Core     -> lift[CyberneticsCore],
      UnitType.Protoss_Fleet_Beacon         -> lift[FleetBeacon],
      UnitType.Protoss_Forge                -> lift[Forge],
      UnitType.Protoss_Photon_Cannon        -> lift[PhotonCannon],
      UnitType.Protoss_Robotics_Facility    -> lift[RoboticsFacility],
      UnitType.Protoss_Robotics_Support_Bay -> lift[RoboticsSupportBay],
      UnitType.Protoss_Stargate             -> lift[Stargate],

      UnitType.Zerg_Creep_Colony            -> lift[CreepColony],
      UnitType.Zerg_Defiler_Mound           -> lift[DefilerMound],
      UnitType.Zerg_Evolution_Chamber       -> lift[EvolutionChamber],
      UnitType.Zerg_Extractor               -> lift[Extractor],
      UnitType.Zerg_Greater_Spire           -> lift[GreaterSpire],
      UnitType.Zerg_Hatchery                -> lift[Hatchery],
      UnitType.Zerg_Hive                    -> lift[Hive],
      UnitType.Zerg_Hydralisk_Den           -> lift[HydraliskDen],
      UnitType.Zerg_Lair                    -> lift[Lair],
      UnitType.Zerg_Infested_Command_Center -> lift[InfestedCommandCenter],
      UnitType.Zerg_Nydus_Canal             -> lift[NydusCanal],
      UnitType.Zerg_Queens_Nest             -> lift[QueensNest],
      UnitType.Zerg_Spawning_Pool           -> lift[SpawningPool],
      UnitType.Zerg_Spire                   -> lift[Spire],
      UnitType.Zerg_Ultralisk_Cavern        -> lift[UltralistCavern],
      UnitType.Zerg_Spore_Colony            -> lift[SporeColony],
      UnitType.Zerg_Sunken_Colony           -> lift[SunkenColony],

      UnitType.Zerg_Infested_Terran -> lift[InfestedTerran],
      UnitType.Zerg_Larva           -> lift[Larva],
      UnitType.Zerg_Egg             -> lift[Egg],
      UnitType.Zerg_Lurker_Egg      -> lift[LurkerEgg],
      UnitType.Zerg_Drone           -> lift[Drone],
      UnitType.Zerg_Broodling       -> lift[Broodling],
      UnitType.Zerg_Zergling        -> lift[Zergling],
      UnitType.Zerg_Defiler         -> lift[Defiler],
      UnitType.Zerg_Guardian        -> lift[Guardian],
      UnitType.Zerg_Hydralisk       -> lift[Hydralisk],
      UnitType.Zerg_Mutalisk        -> lift[Mutalisk],
      UnitType.Zerg_Infested_Terran -> lift[InfestedTerran],
      UnitType.Zerg_Lurker          -> lift[Lurker],
      UnitType.Zerg_Queen           -> lift[Queen],
      UnitType.Zerg_Scourge         -> lift[Scourge],
      UnitType.Zerg_Ultralisk       -> lift[Ultralisk],
      UnitType.Zerg_Overlord        -> lift[Overlord],

      UnitType.Unknown -> lift[Irrelevant]
    )

  def class2UnitType = {
    val stupid = (classOf[Tank], UnitType.Terran_Siege_Tank_Tank_Mode)
    mappingRules.map { case (k, (_, c)) =>
      c -> k
    } + stupid
  }

  def lift(unit: APIUnit) = {
    trace(s"Detected unit of type ${unit.getType}")
    mappingRules.get(unit.getType) match {
      case Some((mapper, _)) => mapper(unit)
      case None              =>
        if (unit.getType.isMineralField) new MineralPatch(unit)
        else new Irrelevant(unit)
    }
  }
}
