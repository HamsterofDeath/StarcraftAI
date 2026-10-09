package pony

import bwapi.{Unit => APIUnit, _}

class Vulture(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with HasSpiderMines with MediumAttackGround with Mechanic
    with MobileRangeWeapon with IsMedium with IsVehicle with ConcussiveGroundDamage with HasSinglePointMagicSpell
    with ArmedMobile {
  override type Caster = Vulture
  override val spells = List(Upgrades.Terran.SpiderMines)
}
