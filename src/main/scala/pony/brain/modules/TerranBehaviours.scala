package pony
package brain
package modules

object TerranBehaviours {
  def allBehaviours(universe: Universe): Seq[DefaultBehaviour[WrapsUnit]] = {
    val allOfThem =
      (new KiteMeleeEnemies(universe) ::
        new StimSelf(universe) ::
        new EnterDefensiveBunker(universe) ::
        new StopMechanic(universe) ::
        new SetupMineField(universe) ::
        new ShieldUnit(universe) ::
        new IrradiateUnit(universe) ::
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
                                          new BlindDetector ::
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
