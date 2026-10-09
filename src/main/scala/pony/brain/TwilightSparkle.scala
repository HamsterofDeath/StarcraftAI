package pony
package brain

import pony.brain.modules.Strategy.Strategies
import pony.brain.modules._

import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

class TwilightSparkle(world: DefaultWorld) {
  self =>
  def renderDebug(renderer: Renderer) = {
    worldDomination.renderDebug(renderer)
  }


  val universe: Universe = new Universe {

    override def pathfinders = new Pathfinders {
      override def groundSafe = myPathFinderGroundSafe

      override def ground = myPathFinderGround

      override def airSafe = myPathFinderAirSafe
    }

    override def bases = self.bases

    override def world = self.world

    override def upgrades = self.upgradeManager

    override def resources = self.resources

    override def unitManager = self.unitManager

    override def currentTick = world.tickCount

    override def mapLayers = self.maps

    override def ownUnits = self.ownUnits

    override def enemyUnits = self.enemyUnits

    override def strategicMap = world.strategicMap

    override def strategy = self.strategy

    override def worldDominationPlan = self.worldDomination

    override def unitGrid = self.unitGrid

    override def ferryManager = self.ferryManager

    override def plugins = aiModules

    override def forces = self.forces
  }
  world.init_!(universe)
  private val ownUnits        = new Units(world.nativeGame, false, universe)
  private val enemyUnits      = new Units(world.nativeGame, true, universe)
  private val bases           = new Bases(world, universe)
  private val resources       = new ResourceManager(universe)
  private val strategy        = new Strategies(universe)
  private val worldDomination = new WorldDominationPlan(universe)
  private val sendOrders      = new SendOrdersToStarcraft(universe)
  // Failed hiring requests live two frames; satisfying them must not wait for a heavy tick.
  private val provideNewUnits = new ProvideNewUnits(universe)
  private val aiModules       = List(
    new DefaultBehaviours(universe),
    new ManageMiningAtBases(universe),
    new ManageMiningAtGeysirs(universe),
    new ProvideSpareSCVs(universe),
    new ProvideNewSupply(universe),
    new ProvideNewBuildings(universe),
    new ProvideSuggestedAndRequestedAddons(universe),
    new HandleDefenses(universe),
    new TerranBunkerDefense(universe),
    new WallWithDepots(universe),
    new CarpetSpread(universe),
    new TankBoxes(universe),
    new FlyFactoriesToNatural(universe),
    new RunTerranCampaign(universe),
    new SetupAntiCloakDefenses(universe),
    new EnqueueFactories(universe),
    new EnqueueArmy(universe),
    new ProvideUpgrades(universe),
    new ProvideExpansions(universe),
    new JobReAssignments(universe),
    AIModule.noop[WrapsUnit](universe)
  )

  private val forces = {
    val game = world.nativeGame
    val me = game.self
    val friends = game.allies()
    val enemies = game.enemies()
    new Forces(me, friends.asScala.toSet, enemies.asScala.toSet, universe)
  }

  private val unitManager            = new UnitManager(universe)
  private val upgradeManager         = new UpgradeManager(universe)
  private val maps                   = new MapLayers(universe)
  private val myPathFinderGround     = LazyVal.from {
    new PathFinder(maps, false, true)
  }
  private val myPathFinderAirSafe    = universe.oncePerTick {
    new PathFinder(maps, true, false)
  }
  private val myPathFinderGroundSafe = universe.oncePerTick {
    new PathFinder(maps, true, true)
  }
  private val unitGrid               = new UnitGrid(universe)
  private val ferryManager           = new FerryManager(universe)

  enemyUnits.registerKill_!(onKillOrCreate)
  ownUnits.registerKill_!(onKillOrCreate)
  enemyUnits.registerAdd_!(onKillOrCreate)
  ownUnits.registerAdd_!(onKillOrCreate)

  def plugins = aiModules

  def pluginByType[T <: AIModule[?] : ClassTag] = {
    val c = implicitly[ClassTag[T]].runtimeClass
    aiModules.find(e => c >= e.getClass).get.asInstanceOf[T]
  }

  def queueOrdersForTick(): Unit = {

    ownUnits.consumeFresh_! {_.init_!(universe)}
    enemyUnits.consumeFresh_! {_.init_!(universe)}

    universe.onTick_!()

    // Cheap per-frame pipeline: job lifecycle, command emission and action bookkeeping must
    // advance every frame so interruptability and order-lock semantics keep their frame meaning.
    unitManager.tick()

    // Satisfy failed hiring requests while they are still alive (they clear after two frames).
    provideNewUnits.onTick_!()

    val tick = world.tickCount
    if (AiCadence.heavyNow(tick)) {
      // Expensive planning runs once per heavy tick (a game second by default).
      maps.tick()
      resources.tick()
      strategy.tick()
      bases.tick()
      worldDomination.onTick_!()
      unitGrid.onTick_!()
      ferryManager.onTick_!()

      val heavyTick = tick / AiCadence.frames
      val activeInThisTick = aiModules.filter(e => tick == 0 || heavyTick % math.max(1, e.onNth / AiCadence.frames) == 0)
      activeInThisTick.flatMap(_.ordersForTick).foreach(world.orderQueue.queue_!)

    }

    // Emit job orders after this frame's planning so freshly hired jobs command their units
    // in the same frame (keeps the order-history liveness check from failing new jobs).
    sendOrders.ordersForTick.foreach(world.orderQueue.queue_!)
    universe.afterTick()

  }

  private def onKillOrCreate = (unit: WrapsUnit) =>
    unit match {
      case _: StaticallyPositioned =>
        myPathFinderGroundSafe.invalidate()
        myPathFinderGround.invalidate()
        myPathFinderAirSafe.invalidate()
      case _ =>
    }
}
