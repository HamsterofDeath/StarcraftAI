package pony

import bwapi.{Unit => APIUnit, _}

class Firebat(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with CanUseStimpack with IsSmall with IsInfantry
    with ArmedMobile with HasSingleTargetSpells with FastAttackGround with ConcussiveGroundDamage {
  override type CasterType = CanUseStimpack
  override val spells = List(Spells.Stimpack)
}
