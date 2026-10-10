package pony
package brain
package modules

import pony.brain.modules.bunkers.EnterDefensiveBunker
import pony.brain.modules.campaign.{GoToInitialPosition, MigrateTowardsPosition, ScoutMap}
import pony.brain.modules.cruisers.{CruiserRaids, EscortWithVessels, YamatoSnipe}
import pony.brain.modules.economy.{DeliverResources, RepairDamagedBuilding, RepairDamagedUnit}
import pony.brain.modules.ferry.TransportGroundUnits
import pony.brain.modules.micro.{
  BlindEnemies, CloakSelfGhost, CloakSelfWraith, Dance, DodgeStorms, EmpShockwave, FocusFire, HelpNearUnits,
  IrradiateUnit, MoveAwayFromDangerousSpotOnAir, MoveAwayFromDangerousSpotOnGround, RangedMicro, SetupMineField,
  ShieldUnit, SiegeUnsiegeSelf, StimSelf, StopMechanic, UseComsat
}
import pony.brain.modules.production.{ContinueInterruptedConstruction, MoveAwayFromConstructionSite, PreventBlockades}
import pony.brain.modules.wall.HoldWallPosts
import pony.units.WrapsUnit

object TerranBehaviours {
  def allBehaviours(universe: Universe): Seq[DefaultBehaviour[WrapsUnit]] = {
    val allOfThem =
      (new RangedMicro(universe) ::
        new StimSelf(universe) ::
        new EnterDefensiveBunker(universe) ::
        new StopMechanic(universe) ::
        new SetupMineField(universe) ::
        new ShieldUnit(universe) ::
        new IrradiateUnit(universe) ::
        new BlindEnemies(universe) ::
        new CruiserRaids(universe) ::
        new DodgeStorms(universe) ::
        new YamatoSnipe(universe) ::
        new EscortWithVessels(universe) ::
        new EmpShockwave(universe) ::
        new HoldWallPosts(universe) ::
        new CloakSelfGhost(universe) ::
        new GoToInitialPosition(universe) ::
        new CloakSelfWraith(universe) ::
        new SiegeUnsiegeSelf(universe) ::
        new MigrateTowardsPosition(universe) ::
        new TransportGroundUnits(universe) ::
        new RepairDamagedUnit(universe) ::
        new RepairDamagedBuilding(universe) ::
        new MoveAwayFromConstructionSite(universe) ::
        new MoveAwayFromDangerousSpotOnGround(universe) ::
        new MoveAwayFromDangerousSpotOnAir(universe) ::
        new PreventBlockades(universe) ::
        new ContinueInterruptedConstruction(universe) ::
        new UseComsat(universe) ::
        new ScoutMap(universe) ::
        /*
                                          new DoNotStray ::
                                          new HealDamagedUnit ::
                                          new FixMedicalProblem ::
         */
        new Dance(universe) ::
        new HelpNearUnits(universe) ::
        new DeliverResources(universe) ::
        new FocusFire(universe) ::
        new ReallyReallyLazy(universe) ::
        Nil)
        .map(_.cast)
    allOfThem
  }
}
