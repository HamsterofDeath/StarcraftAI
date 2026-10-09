package pony

import bwapi.{Unit => APIUnit, _}

class Tank(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with VeryFastAttackGround with Mechanic with CanSiege
    with ArmedMobile with MobileRangeWeapon with IsBig with IsVehicle with ExplosiveGroundDamage
    with HasSingleTargetSpells {

  override type CasterType = Tank
  override val spells = List(Spells.TankSiege)
}
