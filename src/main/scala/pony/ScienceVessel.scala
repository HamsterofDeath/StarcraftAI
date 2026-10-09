package pony

import bwapi.{Unit => APIUnit, _}

class ScienceVessel(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with SupportUnit with CanDetectHidden with Mechanic with HasSingleTargetSpells
    with IsBig with IsShip {
  override type CasterType = ScienceVessel
  override val spells = List(Spells.DefenseMatrix, Spells.Irradiate)
}
