package pony
package units

import pony.combat.{ArmedBuildingCoveringGroundAndAir, GroundAndAirWeapon, NormalAirDamage, NormalGroundDamage}

import bwapi.{Unit => APIUnit, _}

class PhotonCannon(unit: APIUnit)
    extends AnyUnit(unit) with Building with GroundAndAirWeapon with NormalAirDamage with NeedsPower
    with NormalGroundDamage with ArmedBuildingCoveringGroundAndAir with DetectorBuilding {
  override def damageDelayFactorAir = 1

  override def damageDelayFactorGround = 1
}
