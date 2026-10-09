package pony

import bwapi.{Unit => APIUnit, _}

class Templar(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with HasSingleTargetSpells with IsSmall with IsInfantry with CanMorph {
  override val spells = Nil

}
