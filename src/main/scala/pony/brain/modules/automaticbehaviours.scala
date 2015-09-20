package pony
package brain
package modules

import bwapi.Color
import pony.Upgrades.Terran.{GhostCloak, InfantryCooldown, SpiderMines, WraithCloak}

import scala.collection.mutable

abstract class DefaultBehaviour[T <: Mobile : Manifest](override val universe: Universe) extends HasUniverse {
  private val controlled      = new
      collection.mutable.HashMap[Objective, collection.mutable.Set[SingleUnitBehaviour[T]]]
      with mutable.MultiMap[Objective, SingleUnitBehaviour[T]]
  private val unit2behaviour  = mutable.HashMap.empty[T, SingleUnitBehaviour[T]]
  private val controlledUnits = mutable.HashSet.empty[T]

  def priority = SecondPriority.Default

  def refuseCommandsForTicks = 0

  protected def meta = SingleUnitBehaviourMeta(priority, refuseCommandsForTicks)

  def renderDebug(renderer: Renderer): Unit = {}


  def behaviourOf(unit: Mobile) = {
    ifControlsOpt(unit) {identity}
  }
  def ifControlsOpt[R](m: Mobile)(f: (SingleUnitBehaviour[T]) => R) = {
    ifControls(m, Option.empty[R])(e => Some(f(e)))
  }
  def ifControls[R](m: Mobile, ifNot: R)(f: (SingleUnitBehaviour[T]) => R) = {
    if (controls(m)) {
      f(unit2behaviour(assumeSafe(m)))
    } else {
      ifNot
    }
  }
  def controls(unit: Mobile) = {
    canControl(unit) && controlledUnits.contains(assumeSafe(unit))
  }
  def assumeSafe(unit: Mobile): T = unit.asInstanceOf[T]
  def canControl(u: WrapsUnit) = {
    manifest[T].runtimeClass.isAssignableFrom(u.getClass)
  }
  def add_!(u: WrapsUnit, objective: Objective) = {
    assert(canControl(u))
    val behaviour = lift(u.asInstanceOf[T])
    controlled.addBinding(objective, behaviour)
    controlledUnits += behaviour.unit
    unit2behaviour.put(behaviour.unit, behaviour)
  }
  def cast = this.asInstanceOf[DefaultBehaviour[Mobile]]

  protected def lift(t: T): SingleUnitBehaviour[T]

  def onTick(): Unit = {}
}

object Terran {
  def allBehaviours(universe: Universe): Seq[DefaultBehaviour[Mobile]] = {
    val allOfThem = (
                      new StimSelf(universe) ::
                      new StopMechanic(universe) ::
                      new SetupMineField(universe) ::
                      new ShieldUnit(universe) ::
                      new IrradiateUnit(universe) ::
                      new CloakSelfGhost(universe) ::
                      new GoToInitialPosition(universe) ::
                      new CloakSelfWraith(universe) ::
                      /*
                                            new RepairDamagedBuilding ::
                                            new RepairDamagedUnit ::
                                           new RevealHiddenUnitsPermanenly ::
                                           new SetupMineField ::
                                           new Scout ::
                                           new Cloak ::
                                           new DoNotStray ::
                                           new HealDamagedUnit ::
                                           new FixMedicalProblem ::
                                           new BlindDetector ::
                                           new Evade ::
                                           new StopToFire ::
                      */
                      new FocusFire(universe) ::
                      Nil).map(_.cast)
    allOfThem
  }

  class RepairDamagedBuilding(universe: Universe) extends DefaultBehaviour[SCV](universe) {
    override protected def lift(t: SCV): SingleUnitBehaviour[SCV] = ???
  }

  class RepairDamagedUnit(universe: Universe) extends DefaultBehaviour[SCV](universe) {
    override protected def lift(t: SCV): SingleUnitBehaviour[SCV] = ???
  }

  class ContinueInterruptedConstruction(universe: Universe) extends DefaultBehaviour[SCV](universe) {
    override protected def lift(t: SCV): SingleUnitBehaviour[SCV] = ???
  }

  class StimSelf(universe: Universe) extends DefaultBehaviour[CanUseStimpack](universe) {
    override protected def lift(t: CanUseStimpack) = new SingleUnitBehaviour[CanUseStimpack](t, meta) {

      override def preconditionOk = upgrades.hasResearched(InfantryCooldown)

      override def toOrder(what: Objective) = {
        if (t.isAttacking && !t.isStimmed) {
          List(Orders.TechOnSelf(t, InfantryCooldown))
        } else {
          Nil
        }
      }
      override def shortName: String = s"Stim"
    }
  }

  class CloakSelfWraith(universe: Universe) extends DefaultBehaviour[Wraith](universe) {
    override protected def lift(t: Wraith) = new SingleUnitBehaviour[Wraith](t, meta) {

      override def preconditionOk = upgrades.hasResearched(WraithCloak)

      override def toOrder(what: Objective) = {
        if (t.isBeingAttacked && !t.isCloaked) {
          List(Orders.TechOnSelf(t, WraithCloak))
        } else {
          Nil
        }
      }
      override def shortName: String = s"Cloak"
    }
  }

  class CloakSelfGhost(universe: Universe) extends DefaultBehaviour[Ghost](universe) {
    override protected def lift(t: Ghost) = new SingleUnitBehaviour[Ghost](t, meta) {

      override def preconditionOk = upgrades.hasResearched(GhostCloak)

      override def toOrder(what: Objective) = {
        if (t.isBeingAttacked && !t.isCloaked) {
          List(Orders.TechOnSelf(t, GhostCloak))
        } else {
          Nil
        }
      }
      override def shortName: String = s"C"
    }
  }

  class FormationHelper(override val universe: Universe, distance: Int = 0) extends HasUniverse {
    def allOutsideNonBlacklisted = {
      val map = mapLayers.reallyFreeBuildingTiles
      defenseLines.iterator.flatMap(_.pointsOutside).filterNot(blacklisted).filter(map.free)
    }

    def allInsideNonBlacklisted = {
      val map = mapLayers.reallyFreeBuildingTiles
      defenseLines.iterator.flatMap(_.pointsInside).filterNot(blacklisted).filter(map.free)
    }

    def blacklisted(e: MapTilePosition) = blocked.contains(e)

    def cleanBlacklist(dispose: (MapTilePosition, BlacklistReason) => Boolean) = {
      blocked.filter(e => dispose(e._1, e._2))
      .foreach(e => whiteList_!(e._1))
    }

    private val myDefenseLines = LazyVal.from {
      bases.bases.flatMap { base =>
        strategicMap.defenseLineOf(base).map(_.tileDistance(distance))
      }
    }

    universe.bases.register((base: Base) => {
      myDefenseLines.invalidate()
    })

    def defenseLines = myDefenseLines.get

    def blackList_!(tile: MapTilePosition): Unit = {
      blocked.put(tile, BlacklistReason(universe.currentTick))
    }

    def whiteList_!(tilePosition: MapTilePosition): Unit = {
      blocked.remove(tilePosition)
    }

    def reasonForBlacklisting(tilePosition: MapTilePosition) = blocked.get(tilePosition)

    private val blocked = mutable.HashMap.empty[MapTilePosition, BlacklistReason]

    case class BlacklistReason(when: Int)

  }

  class GoToInitialPosition(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    private val helper = new FormationHelper(universe)

    private val ignore = mutable.HashSet.empty[Mobile]

    override protected def lift(t: Mobile): SingleUnitBehaviour[Mobile] = new SingleUnitBehaviour[Mobile](t, meta) {
      override def shortName: String = "IP"
      override def toOrder(what: Objective) = {
        if (universe.time.minutes <= 4 || ignore(t) || t.isBeingCreated) {
          Nil
        } else {
          helper.allInsideNonBlacklisted.toStream.headOption.map { where =>
            ignore += t
            helper.blacklisted(where)
            Orders.AttackMove(t, where)
          }.toList
        }
      }
    }
  }

  class UseComsat(universe: Universe) extends DefaultBehaviour[MobileDetector](universe) {
    override protected def lift(t: MobileDetector): SingleUnitBehaviour[MobileDetector] = ???
  }

  class SetupMineField(universe: Universe) extends DefaultBehaviour[Vulture](universe) {

    private val helper = new FormationHelper(universe, 2)

    override def renderDebug(renderer: Renderer): Unit = {
      helper.defenseLines.foreach { defLine =>
        defLine.pointsOutside.foreach { tile =>
          renderer.in_!(Color.White).drawCircleAroundTile(tile)
        }
      }
    }

    override def onTick(): Unit = {
      super.onTick()
      ifNth(137) {
        helper.cleanBlacklist((_, reason) => reason.when + 240 < universe.currentTick)
      }
    }

    override def priority = SecondPriority.Less

    override protected def lift(t: Vulture): SingleUnitBehaviour[Vulture] = new SingleUnitBehaviour[Vulture](t, meta) {

      trait State
      case object Idle extends State
      case class DroppingMine(tile: MapTilePosition) extends State

      private var state: State            = Idle
      private var originalSpiderMineCount = t.spiderMineCount

      override def shortName: String = "LM"
      override def toOrder(what: Objective) = {
        val (newState, orders) = state match {
          case Idle =>
            def beLazy = Idle -> Nil
            // TODO include test in tech trait
            if (t.spiderMineCount > 0 && t.canCastNow(SpiderMines)) {
              val candiates = helper.allOutsideNonBlacklisted
              val dropMineHere = candiates.minByOpt(_.distanceToSquared(t.currentTile))
              dropMineHere.foreach(helper.blackList_!)
              dropMineHere.map { where =>
                DroppingMine(where) -> t.toOrder(SpiderMines, where).toList
              }.getOrElse(beLazy)
            } else {
              beLazy
            }

          case myState@DroppingMine(where) if t.canCastNow(SpiderMines) =>
            myState -> t.toOrder(SpiderMines, where).toList
          case DroppingMine(_) if t.spiderMineCount < originalSpiderMineCount =>
            originalSpiderMineCount = t.spiderMineCount
            Idle -> Nil
          case myState@DroppingMine(_) =>
            myState -> Nil
        }
        state = newState
        orders
      }
      override def preconditionOk: Boolean = universe.upgrades.hasResearched(Upgrades.Terran.SpiderMines)
    }
  }

  class Scout(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    override protected def lift(t: Mobile): SingleUnitBehaviour[Mobile] = ???
  }

  class DoNotStray(universe: Universe) extends DefaultBehaviour[SupportUnit](universe) {
    override protected def lift(t: SupportUnit): SingleUnitBehaviour[SupportUnit] = ???
  }

  class OneTimeUnitSpellCast[C <: HasSingleTargetSpells : Manifest, T <: Mobile : Manifest](universe: Universe,
                                                                                            spell:
                                                                                            SingleTargetSpell[C, T])
    extends DefaultBehaviour[C](universe) {
    private val helper = NonConflictingTargetPicks.forSpell(spell, universe)

    override def refuseCommandsForTicks = 10

    override def priority: SecondPriority = SecondPriority.Max

    override protected def lift(t: C): SingleUnitBehaviour[C] = new SingleUnitBehaviour[C](t, meta) {
      override def shortName: String = s"Cast ${spell.getClass.className}"
      override def toOrder(what: Objective): Seq[UnitOrder] = {
        if (t.canCastNow(spell.tech)) {
          val h = helper
          h.suggestTargetFor(t).map { target =>
            h.notifyLock_!(t, target)
            t.toOrder(spell.tech, target)
          }.toList
        } else {
          Nil
        }
      }
    }
  }

  class StopMechanic(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Lockdown)
  class ShieldUnit(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.DefenseMatrix)
  class IrradiateUnit(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Irradiate)
  class CloakWraith(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Irradiate)

  class HealDamagedUnit(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class FixMedicalProblem(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class BlindDetector(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class Evade(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    override protected def lift(t: Mobile): SingleUnitBehaviour[Mobile] = ???
  }
  class FocusFire(universe: Universe) extends DefaultBehaviour[MobileRangeWeapon](universe) {

    private val helper = new FocusFireOrganizer(universe)

    override protected def lift(t: MobileRangeWeapon): SingleUnitBehaviour[MobileRangeWeapon] = new
        SingleUnitBehaviour(t, meta) {
      override def shortName = "FF"
      override def toOrder(what: Objective) = {
        helper.suggestTarget(t).map { target =>
          Orders.AttackUnit(t, target)
        }.toList
      }
    }
  }
}

case class Target[T <: Mobile](caster: HasSingleTargetSpells, target: T)

class FocusFireOrganizer(override val universe: Universe) extends HasUniverse {

  universe.register_!(() => {
    enemy2Attackers.filter(_._1.isDead).foreach { case (dead, attackers) =>
      trace(s"Unit $dead died, reorganizing attackers")
      enemy2Attackers.remove(dead)
      attackers.allAttackers.foreach { unit =>
        me2Enemy.remove(unit)
      }
    }

    me2Enemy.keySet.filter(_.isDead).foreach { dead =>
      val target = me2Enemy.remove(dead)
      target.foreach { canDie =>
        enemy2Attackers(canDie).removeAttacker_!(dead)
      }
    }
  })

  universe.register_!(() => {
    invalidateQueue()
  })

  class Attackers(val target: CanDie) {
    def isOutOfRange(myUnit: MobileRangeWeapon) = !myUnit.isInWeaponRange(target)

    def recalculatePlannedDamage_!(): Unit = {
      val attackersSorted = plannedDamage.keys.toArray.sortBy(_.cooldownTimer)
      val damage = new MutableHP(0, 0)
      val currentHp = actualHP
      val assumeHp = new MutableHP(currentHp.hitpoints, currentHp.shield)
      attackersSorted.foreach { attacker =>
        val shotCount = 1 + attacker.assumeShotDelayOn(target)
        val moreDamage = attacker.calculateDamageOn(target, assumeHp.hp, assumeHp.shields, shotCount)
        damage +! moreDamage
        assumeHp -! moreDamage
      }
      plannedDamageMerged = damage
    }

    def removeAttacker_!(t: MobileRangeWeapon): Unit = {
      attackers -= t
      plannedDamage -= t
      recalculatePlannedDamage_!()
      invalidateQueue()
    }

    def canSpare(attacker: MobileRangeWeapon) = {
      assert(isAttacker(attacker))
      // this is not entirely correct because of shields & zerg regeneration, but should be close enough
      val damage = plannedDamage(attacker)

      actualHP <(plannedDamageMerged.hp - damage.onHp, plannedDamageMerged.shields - damage.onShields)
    }

    def allAttackers = attackers.iterator

    def isAttacker(t: MobileRangeWeapon) = attackers(t)

    def addAttacker_!(t: MobileRangeWeapon): Unit = {
      assert(!attackers(t), s"$attackers already contains $t")
      attackers += t
      // for slow attacks, we assume that one is always on the way to hit to avoid overkill
      val expectedDamage = {
        val factor = 1 + t.assumeShotDelayOn(target)

        t.calculateDamageOn(target, hpAfterNextAttacks.hp, hpAfterNextAttacks.shields, factor)
      }

      plannedDamage.put(t, expectedDamage)
      recalculatePlannedDamage_!()
      hpAfterNextAttacks -! expectedDamage
    }

    def isOverkill = actualHP <(plannedDamageMerged.hp, plannedDamageMerged.shields)

    class MutableHP(var hp: Int, var shields: Int) {
      def toHP = HitPoints(hp, shields)

      def +!(damageDone: DamageSingleAttack) = {
        hp += damageDone.onHp
        shields += damageDone.onShields
      }

      assert(shields >= 0)

      def alive = hp > 0

      def -!(dsa: DamageSingleAttack) = {
        hp -= dsa.onHp
        shields -= dsa.onShields
      }
    }

    private val attackers = mutable.HashSet.empty[MobileRangeWeapon]
    private def actualHP = target.hitPoints
    private val plannedDamage       = mutable.HashMap.empty[MobileRangeWeapon, DamageSingleAttack]
    private var plannedDamageMerged = new MutableHP(0, 0)
    private val hpAfterNextAttacks  = new
        MutableHP(actualHP.hitpoints, actualHP.shield)

    def canTakeMore = hpAfterNextAttacks.alive
  }

  def invalidateQueue(): Unit = {
    prioritizedTargets.invalidate()
  }
  private val enemy2Attackers    = mutable.HashMap.empty[CanDie, Attackers]
  private val me2Enemy           = mutable.HashMap.empty[MobileRangeWeapon, CanDie]
  private val prioritizedTargets = LazyVal.from {
    val prioritized = universe.enemyUnits.allCanDie.toVector.sortBy { e =>
      (e.isHarmlessNow.ifElse(1, 0), enemy2Attackers.contains(e).ifElse(0, 1), -e.price.sum)
    }
    prioritized
  }

  def suggestTarget(myUnit: MobileRangeWeapon): Option[CanDie] = {
    val maybeAttackers = me2Enemy.get(myUnit)
                         .map(enemy2Attackers)
    val shouldLeaveTeam = {
      def outOfRange = maybeAttackers.exists(_.isOutOfRange(myUnit))
      def overkill = maybeAttackers
                     .filter(_.isOverkill)
                     .exists(_.canSpare(myUnit))
      outOfRange || overkill
    }
    if (shouldLeaveTeam) {
      maybeAttackers.foreach(_.removeAttacker_!(myUnit))
      me2Enemy.remove(myUnit)
    }

    prioritizedTargets.get.find { target =>
      val existing = enemy2Attackers.get(target).exists(_.isAttacker(myUnit))
      existing || (myUnit.isInWeaponRange(target) && myUnit.canAttack(target) &&
                   enemy2Attackers.getOrElseUpdate(target, new Attackers(target)).canTakeMore)
    }.foreach { attackThis =>
      val plan = enemy2Attackers(attackThis)
      if (!plan.isAttacker(myUnit)) {
        me2Enemy.put(myUnit, attackThis)
        plan.addAttacker_!(myUnit)
      }
      invalidateQueue()
    }
    me2Enemy.get(myUnit)
  }
}

object NonConflictingTargetPicks {
  def forSpell[T <: HasSingleTargetSpells, M <: Mobile : Manifest](spell: SingleTargetSpell[T, M],
                                                                   universe: Universe) = {

    new NonConflictingTargetPicks(spell, {
      case x: M if spell.canBeCastOn(x) & spell.shouldActivateOn(x) => x
    },
    spell.isAffected, universe)
  }
}

class NonConflictingTargetPicks[T <: HasSingleTargetSpells, M <: Mobile : Manifest](spell: SingleTargetSpell[T, M],
                                                                                    targetConstraint:
                                                                                    PartialFunction[Mobile, M],
                                                                                    keepLocked: M => Boolean,
                                                                                    override val universe: Universe)
  extends HasUniverse {
  def afterTick(): Unit = {
    prioritizedTargets.invalidate()
    locked.filterNot(keepLocked).foreach { elem =>
      unlock_!(elem)
    }
  }

  universe.register_!(() => {
    afterTick()
  })

  def notifyLock_!(t: T, target: M): Unit = {
    locked += target
    val tar = Target(t, target)
    lockedTargets += tar
    assignments.put(target, tar)
    prioritizedTargets.invalidate()
  }

  private def unlock_!(target: M): Unit = {
    locked -= target
    val old = assignments.remove(target).get
    lockedTargets -= old
    prioritizedTargets.invalidate()
  }

  private val locked        = mutable.HashSet.empty[M]
  private val lockedTargets = mutable.HashSet.empty[Target[M]]
  private val assignments   = mutable.HashMap.empty[M, Target[M]]

  private val prioritizedTargets = LazyVal.from {
    val base = {
      val targets = {
        if (spell.castOn == EnemyUnits) {
          universe.enemyUnits.allByType[M]
        } else {
          universe.myUnits.allByType[M]
        }
      }
      targets.collect(targetConstraint)
    }

    spell.priorityRule.fold(base.toVector) { rule =>
      base.toVector.sortBy(m => -rule(m))
    }
  }

  def suggestTargetFor(caster: T): Option[M] = {
    // for now, just pick the first in range that is not yet taken
    val range = spell.castRangeSquare

    val filtered = prioritizedTargets.get.filterNot(locked)
    filtered.find {_.currentPosition.distanceToSquared(caster.currentPosition) < range}
  }
}



