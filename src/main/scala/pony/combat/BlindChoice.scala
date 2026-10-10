package pony
package combat

import pony.units.Mobile

import bwapi.UnitType

/**
  * Which enemies are worth an Optical Flare, and in which order. Blindness lasts for the target's whole life and only a
  * Terran medic can cure it, so a bot that flares every enemy in reach leaves the opposing army seeing about a tile:
  * its ranged units only shoot what an unblinded ally or building sees, and blinded detectors detect nothing.
  */
private[pony] object BlindChoice {

  /** What the choice needs to know about one enemy unit. */
  final case class Candidate(
      detector: Boolean,
      worker: Boolean,
      disposable: Boolean,
      price: Int,
      health: Double,
      ranged: Boolean
  )

  /** Below this share of hit points and shields a unit is likely to die before its blindness pays off. */
  val MinHealth = 0.25

  /** Detectors and real fighters; never workers, short-lived units (interceptors, eggs, hallucinations...) or the dying. */
  def worth(c: Candidate) = c.health >= MinHealth && (c.detector || (!c.worker && !c.disposable))

  /** Detectors first, then the costliest, healthiest units; a blinded ranged unit loses most of its reach. */
  def score(c: Candidate): Double =
    (if (c.detector) 100000.0 else 0.0) + c.price * c.health * (if (c.ranged) 2.0 else 1.0)

  private val ShortLived = Set(
    UnitType.Protoss_Interceptor,
    UnitType.Protoss_Scarab,
    UnitType.Terran_Vulture_Spider_Mine,
    UnitType.Zerg_Larva,
    UnitType.Zerg_Egg,
    UnitType.Zerg_Lurker_Egg,
    UnitType.Zerg_Cocoon,
    UnitType.Zerg_Broodling
  )

  /** Carriers and reavers fight at range through interceptors and scarabs; their own weapon reach says nothing. */
  private val RangedWithoutWeapon = Set(UnitType.Protoss_Carrier, UnitType.Protoss_Reaver)

  def of(m: Mobile): Candidate = {
    val unit      = m.nativeUnit
    val kind      = unit.getType
    val maxHealth = kind.maxHitPoints + kind.maxShields
    Candidate(
      detector = kind.isDetector,
      worker = kind.isWorker,
      disposable = unit.isHallucination || ShortLived(kind),
      price = kind.mineralPrice + kind.gasPrice,
      health = if (maxHealth <= 0) 1.0 else (unit.getHitPoints + unit.getShields).toDouble / maxHealth,
      ranged = kind.groundWeapon.maxRange > 32 || kind.airWeapon.maxRange > 32 || RangedWithoutWeapon(kind)
    )
  }
}
