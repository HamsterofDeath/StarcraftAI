package pony
package units

import pony.combat.{
  ArmedMobile, ConcussiveGroundDamage, GroundWeapon, HasSinglePointMagicSpell, MediumAttackGround, MobileRangeWeapon
}
import pony.tech.Upgrades

import bwapi.{Unit => APIUnit, _}

class Vulture(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with HasSpiderMines with MediumAttackGround with Mechanic
    with MobileRangeWeapon with IsMedium with IsVehicle with ConcussiveGroundDamage with HasSinglePointMagicSpell
    with ArmedMobile {
  override type Caster = Vulture
  override val spells = List(Upgrades.Terran.SpiderMines)
}
