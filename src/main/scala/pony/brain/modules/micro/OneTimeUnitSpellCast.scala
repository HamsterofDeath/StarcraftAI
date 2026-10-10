package pony
package brain
package modules
package micro

import pony.combat.{HasSingleTargetSpells, SingleTargetSpell}
import pony.units.Mobile

import scala.reflect.ClassTag

class OneTimeUnitSpellCast[C <: HasSingleTargetSpells: ClassTag, T <: Mobile: ClassTag](
    universe: Universe,
    spell: SingleTargetSpell[C, T]
) extends DefaultBehaviour[C](universe) {
  private val helper = NonConflictingSpellTargets.forSpell(spell, universe)

  universe.register_!(() => {
    helper.afterTick()
  })

  override def onTick_!() = {
    super.onTick_!()
  }

  override def refuseCommandsForTicks = 12

  override def priority: SecondPriority = SecondPriority.Max

  override protected def wrapBase(unit: C): SingleUnitBehaviour[C] = new SingleUnitBehaviour[C](unit, meta) {
    override def describeShort: String = s"Cast ${spell}"

    // spells researched at a building wait for it; the rest (defensive matrix) are there from the start
    override def preconditionOk =
      spell.tech.nativeTech.whatResearches == bwapi.UnitType.None || upgrades.hasResearched(spell.tech)

    override def toOrder(what: Objective): Seq[UnitOrder] = {
      if (this.unit.canCastNow(spell.tech)) {
        val h = helper
        h.suggestTargetFor(this.unit).map { target =>
          h.notifyLock_!(this.unit, target)
          NativeMatchEvidence.trace(
            "spell-cast",
            s"spell=${spell} caster=${this.unit.nativeUnitId} " +
              s"target=${target.nativeUnit.getType} id=${target.nativeUnitId} energy=${this.unit.mana}"
          )
          this.unit.toOrder(spell.tech, target)
        }.toList
      } else {
        Nil
      }
    }
  }
}
