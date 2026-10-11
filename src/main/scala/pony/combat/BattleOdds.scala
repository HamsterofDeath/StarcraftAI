package pony
package combat

import bwapi.{DamageType, UnitSizeType, UnitType, WeaponType}

/**
  * Who wins a fight, by Lanchester's square law as a battery of 400 random battles taught it (LearnedSquareLaw): a
  * side's strength is its fire against the other side's mix of hit points, times its own hit points, and the chance to
  * win grows with the log of our strength over theirs. A matchup or a type the battery never saw comes from the unit
  * stats. Cross-validated on the battery the law called 90 percent of the battles right; its bias says the bot's
  * micro wins an even fight three times in four against the Protoss AI.
  *
  * A side is a list of fighters, each its type and the share of its hit points and shields left: a hurt unit fires as
  * hard as a whole one but falls sooner.
  */
object BattleOdds {
  import LearnedSquareLaw.{Bias, Dps, Durability, Scale}

  type Fighter = (UnitType, Double)

  /** The chance that `us` destroy `them`. */
  def winChance(us: Seq[Fighter], them: Seq[Fighter]): Double =
    if (hitPoints(us) <= 0) 0.0
    else if (hitPoints(them) <= 0) 1.0
    else 1 / (1 + math.exp(-(Scale * advantage(us, them) + Bias)))

  /**
    * The winner keeps the square root of one less the loser's share of its strength: our surviving share when we win,
    * less theirs when they do, in [-1, 1]. The bot's edge (the learned bias) counts as strength.
    */
  def margin(us: Seq[Fighter], them: Seq[Fighter]): Double =
    if (hitPoints(us) <= 0) -1.0
    else if (hitPoints(them) <= 0) 1.0
    else {
      val z = advantage(us, them) + Bias / Scale
      math.signum(z) * math.sqrt(1 - math.exp(-math.abs(z)))
    }

  /** The log of our strength over theirs. */
  def advantage(us: Seq[Fighter], them: Seq[Fighter]): Double =
    math.log(strength(us, them) + Tiny) - math.log(strength(them, us) + Tiny)

  /** Damage a second of `attacker` against `target`: as learned where the battery saw the matchup. */
  def dps(attacker: UnitType, target: UnitType): Double =
    Dps.getOrElse((attacker, target), statsDps(attacker, target))

  /** Hit points and shields `t` stands for: as learned where the battery saw the type. */
  def durability(t: UnitType): Double = Durability.getOrElse(t, statsDurability(t))

  /** Whether `attacker` can hurt any of `targets`. */
  def canHurt(attacker: UnitType, targets: Iterable[UnitType]): Boolean = targets.exists(dps(attacker, _) > 0)

  private val Tiny = 1e-6

  private def hitPoints(side: Seq[Fighter]) = side.iterator.map((t, health) => durability(t) * health).sum

  private def strength(side: Seq[Fighter], other: Seq[Fighter]) = {
    val enemy = other.groupMapReduce(_._1)((t, health) => durability(t) * health)(_ + _)
    val total = enemy.values.sum
    if (total <= 0) 0.0
    else {
      val fire = side.groupMapReduce(_._1)(_ => 1)(_ + _).iterator.map { (t, count) =>
        count * enemy.iterator.map((target, hp) => dps(t, target) * hp).sum / total
      }.sum
      fire * hitPoints(side)
    }
  }

  /** Interceptors and scarabs count with their carrier and reaver. */
  private def ammunition(t: UnitType) = t == UnitType.Protoss_Interceptor || t == UnitType.Protoss_Scarab

  private val Interceptors = 8

  private[pony] def statsDurability(t: UnitType): Double =
    if (ammunition(t)) 0.0 else (t.maxHitPoints + t.maxShields).toDouble

  /**
    * Damage a second after armor and size, from the stats. BWAPI's firebat lands three hits an attack where the game
    * deals two; a carrier fires through its interceptors and a reaver through its scarabs, whose cooldowns BWAPI gives
    * as a frame; a bunker holds four marines.
    */
  private[pony] def statsDps(attacker: UnitType, target: UnitType): Double =
    if (ammunition(target) || ammunition(attacker)) 0.0
    else
      attacker match {
        case UnitType.Protoss_Carrier => Interceptors * perHit(6, DamageType.Normal, target) * FramesPerSecond / 30
        case UnitType.Protoss_Reaver  =>
          if (target.isFlyer) 0.0 else perHit(100, DamageType.Normal, target) * FramesPerSecond / 60
        case UnitType.Terran_Bunker => 4 * statsDps(UnitType.Terran_Marine, target)
        case _                      =>
          val flies  = target.isFlyer
          val weapon = if (flies) attacker.airWeapon else attacker.groundWeapon
          val hits   =
            if (attacker == UnitType.Terran_Firebat) 2
            else weapon.damageFactor * (if (flies) attacker.maxAirHits else attacker.maxGroundHits)
          if (weapon == WeaponType.None || weapon.damageCooldown <= 0) 0.0
          else perHit(weapon.damageAmount, weapon.damageType, target) * hits * FramesPerSecond / weapon.damageCooldown
      }

  private val FramesPerSecond = 24.0

  private def perHit(damage: Int, kind: DamageType, target: UnitType) = {
    val factor = (kind, target.size) match {
      case (DamageType.Concussive, UnitSizeType.Medium) => 0.5
      case (DamageType.Concussive, UnitSizeType.Large)  => 0.25
      case (DamageType.Explosive, UnitSizeType.Small)   => 0.5
      case (DamageType.Explosive, UnitSizeType.Medium)  => 0.75
      case _                                            => 1.0
    }
    math.max(0.5, damage * factor - target.armor)
  }
}
