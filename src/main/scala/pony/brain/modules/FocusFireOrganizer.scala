package pony
package brain
package modules

import bwapi.Color

import scala.collection.mutable
import scala.reflect.ClassTag

class FocusFireOrganizer(override val universe: Universe) extends HasUniverse {
  def renderDebug_!(renderer: Renderer): Unit = {
    mine2Enemy.foreach({ case (from, to) =>
      renderer.in_!(Color.White).drawLine(from.center, to.center)
    })
  }

  private val mine2Plan          = mutable.HashMap.empty[MobileRangeWeapon, Attackers]
  private val enemy2Mine         = mutable.HashMap.empty[CanDie, Attackers]
  private val mine2Enemy         = mutable.HashMap.empty[MobileRangeWeapon, CanDie]
  private val prioritizedTargets = {
    LazyVal.from {
      val prioritized = {
        universe.enemyUnits
          .allCanDie
          .iterator
          .filter(_.isAttackable)
          .toVector
          .sortBy { e =>
            (
              e.isHarmlessNow.ifElse(1, 0),
              enemy2Mine.contains(e).ifElse(0, 1),
              -e.price.sum,
              e.hitPoints.sum
            )
          }
      }
      prioritized
    }
  }

  private def consistent = {
    //    enemy2Mine.valuesIterator.foreach { attackers =>
    //      val m2e = attackers.allAttackers.map { e =>
    //        assert(mine2Enemy.contains(e))
    //        e -> mine2Enemy(e)
    //      }
    //      m2e.foreach { case (m,e) =>
    //        assert(mine2Enemy(m) == e)
    //      }
    //    }
    true
  }

  override def onTick_!() = {
    super.onTick_!()
    enemy2Mine.filter(_._1.isDead).foreach { case (dead, attackers) =>
      trace(s"Unit $dead died, reorganizing attackers")
      enemy2Mine.remove(dead)
      attackers.allAttackers.foreach { unit =>
        mine2Enemy.remove(unit)
      }
    }

    mine2Enemy.keySet.filter(_.isDead).foreach { dead =>
      val target = mine2Enemy.remove(dead)
      target.foreach { canDie =>
        enemy2Mine(canDie).removeAttacker_!(dead)
      }
    }

    enemy2Mine.valuesIterator.foreach(_.onTick_!())

    invalidateQueue()
  }

  def invalidateQueue(): Unit = {
    prioritizedTargets.invalidate()
  }

  def suggestTarget(myUnit: MobileRangeWeapon): Option[CanDie] = {
    val myCurrentTarget = mine2Enemy.get(myUnit)
    myCurrentTarget.foreach { t =>
      val maybeAttackers  = enemy2Mine(t)
      val shouldLeaveTeam = {
        def outOfRange = maybeAttackers.isOutOfRange(myUnit)
        def overkill   = maybeAttackers.isOverkill && maybeAttackers.canSpare(myUnit)
        outOfRange || overkill
      }
      if (shouldLeaveTeam) {
        maybeAttackers.removeAttacker_!(myUnit)
        mine2Enemy.remove(myUnit)
      }
    }

    val bestTarget = prioritizedTargets.find { target =>
      val existing = enemy2Mine.get(target).exists(_.isAttacker(myUnit))
      assert(!existing || myCurrentTarget.isDefined)
      existing ||
      (myUnit.canAttackIfNear(target) && myUnit.isInWeaponRangeExact(target) &&
        enemy2Mine.getOrElseUpdate(target, new Attackers(target)).canTakeMore)
    }
    bestTarget.foreach { attackThis =>
      val oldPlan = mine2Plan.get(myUnit)
      oldPlan.foreach { plan =>
        if (plan.isAttacker(myUnit)) {
          plan.removeAttacker_!(myUnit)
        }
        mine2Plan.remove(myUnit)
      }
      val plan = enemy2Mine(attackThis)
      if (plan.isAttacker(myUnit)) {
        assert(mine2Enemy.contains(myUnit))
      } else {
        mine2Enemy.put(myUnit, attackThis)
        plan.addAttacker_!(myUnit)
        mine2Plan.put(myUnit, plan)
      }
      invalidateQueue()
    }
    mine2Enemy.get(myUnit)
  }

  class Attackers(val target: CanDie) {

    private val attackers     = mutable.HashSet.empty[MobileRangeWeapon]
    private val plannedDamage =
      mutable.HashMap.empty[MobileRangeWeapon, DamageSingleAttack]
    private val hpAfterNextAttacks  = currentHp
    private val plannedDamageMerged = new MutableHP(0, 0)

    def isOutOfRange(myUnit: MobileRangeWeapon) = !myUnit.isInWeaponRangeExact(target)

    def removeAttacker_!(t: MobileRangeWeapon): Unit = {
      assert(attackers(t))
      assert(plannedDamage.contains(t))
      attackers -= t
      plannedDamage -= t
      recalculatePlannedDamage_!()
      hpAfterNextAttacks.set(currentHp -! plannedDamageMerged)
      invalidateQueue()
    }

    def onTick_!(): Unit = {
      // adjust to reality
      hpAfterNextAttacks.set(currentHp -! plannedDamageMerged)
    }

    private def currentHp = new MutableHP(actualHP.hitpoints, actualHP.shield)

    def canSpare(attacker: MobileRangeWeapon) = {
      assert(isAttacker(attacker))
      // this is not entirely correct because of shields & zerg regeneration, but should be
      // close enough
      val damage = plannedDamage(attacker)

      val predicted = (
        plannedDamageMerged.hitPoints - damage.onHp,
        plannedDamageMerged.shieldPoints - damage.onShields
      )
      actualHP < predicted
    }

    def isAttacker(t: MobileRangeWeapon) = attackers(t)

    def allAttackers = attackers.iterator

    def addAttacker_!(t: MobileRangeWeapon): Unit = {
      assert(!attackers(t), s"$attackers already contains $t")
      assert(enemy2Mine(target) == this)
      attackers += t
      // for slow attacks, we assume that one is always on the way to hit to avoid overkill
      val expectedDamage = {
        val factor = 1 + t.assumeShotDelayOn(target)

        t.calculateDamageOn(
          target,
          hpAfterNextAttacks.hitPoints,
          hpAfterNextAttacks.shieldPoints,
          factor
        )
      }

      plannedDamage.put(t, expectedDamage)
      recalculatePlannedDamage_!()
      hpAfterNextAttacks -! expectedDamage
    }

    def recalculatePlannedDamage_!(): Unit = {
      val attackersSorted = plannedDamage.keys.toArray.sortBy(_.cooldownTimer)
      val damage          = new MutableHP(0, 0)
      val currentHp       = actualHP
      val assumeHp        = new MutableHP(currentHp.hitpoints, currentHp.shield)
      attackersSorted.foreach { attacker =>
        val shotCount  = 1 + attacker.assumeShotDelayOn(target)
        val moreDamage = attacker
          .calculateDamageOn(
            target,
            assumeHp.hitPoints,
            assumeHp.shieldPoints,
            shotCount
          )
        damage +! moreDamage
        assumeHp -! moreDamage
      }
      plannedDamageMerged.set(damage)
    }

    def isOverkill = actualHP < (plannedDamageMerged.hitPoints, plannedDamageMerged.shieldPoints)

    private def actualHP = target.hitPoints

    def canTakeMore = hpAfterNextAttacks.alive

    class MutableHP(var hitPoints: Int, var shieldPoints: Int) extends HasHpAndShields {
      override def hp = hitPoints

      override def shields = shieldPoints

      def toHP = HitPoints(hitPoints, shieldPoints)

      def set(hp: MutableHP): Unit = {
        this.hitPoints = hp.hitPoints
        shieldPoints = hp.shieldPoints
      }

      def +!(damageDone: HasHpAndShields) = {
        hitPoints += damageDone.hp
        shieldPoints += damageDone.shields
        this
      }

      assert(shieldPoints >= 0)

      def alive = hitPoints > 0

      def -!(dsa: HasHpAndShields) = {
        hitPoints -= dsa.hp
        shieldPoints -= dsa.shields
        this
      }

    }

    override def toString = s"Attackers($attackers)"
  }

}
