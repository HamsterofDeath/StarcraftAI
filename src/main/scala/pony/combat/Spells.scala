package pony
package combat

import pony.tech.Upgrades
import pony.units.{CanUseStimpack, Ghost, Mechanic, Medic, Mobile, Organic, ScienceVessel, Tank, Wraith}

object Spells {

  case object Lockdown extends SingleTargetSpell[Ghost, Mechanic](Upgrades.Terran.GhostStop) {
    override def isAffected(m: Mechanic) = {
      m.isLocked
    }
  }

  case object WraithCloak extends SingleTargetSpell[Wraith, Wraith](Upgrades.Terran.WraithCloak) {
    override def isAffected(m: Wraith) = {
      m.isCloaked
    }
  }

  case object TankSiege extends SingleTargetSpell[Tank, Tank](Upgrades.Terran.TankSiegeMode) {
    override def isAffected(m: Tank) = {
      m.isSieged
    }
  }

  case object Stimpack
      extends SingleTargetSpell[CanUseStimpack, CanUseStimpack](Upgrades.Terran.InfantryCooldown) {
    override def isAffected(m: CanUseStimpack) = {
      m.isStimmed
    }
  }

  case object Irradiate
      extends SingleTargetSpell[ScienceVessel, Organic](Upgrades.Terran.Irradiate) {
    override def isAffected(m: Organic) = {
      m.isIrradiated
    }
  }

  case object DefenseMatrix
      extends SingleTargetSpell[ScienceVessel, Mobile](Upgrades.Terran.Defensematrix) {
    override def isAffected(m: Mobile) = m.matrixHp >= 25 // allow refreshing the matrix

    override def castOn = OwnUnits

    override def shouldActivateOn(validated: Mobile) = {
      validated.isBeingAttacked
    }
  }

  /** Optical Flare on every enemy BlindChoice rates worth it, best first; any unit but a building can be blinded. */
  case object Blind extends SingleTargetSpell[Medic, Mobile](Upgrades.Terran.MedicFlare) {
    override def isAffected(m: Mobile) = m.nativeUnit.isBlind

    override def shouldActivateOn(m: Mobile) = BlindChoice.worth(BlindChoice.of(m))

    override def priorityRule = Some((m: Mobile) => BlindChoice.score(BlindChoice.of(m)))
  }

}
