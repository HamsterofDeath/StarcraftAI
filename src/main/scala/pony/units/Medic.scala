package pony
package units

import pony.combat.{HasSingleTargetSpells, Spells}

import bwapi.{Unit => APIUnit, _}

class Medic(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with SupportUnit with HasSingleTargetSpells with IsSmall with IsInfantry {
  override type CasterType = Medic
  override val spells = List(Spells.Blind)
}
