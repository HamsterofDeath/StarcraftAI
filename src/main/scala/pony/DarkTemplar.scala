package pony

import bwapi.{Unit => APIUnit, _}

class DarkTemplar(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with CanCloak with IsSmall with IsInfantry with ArmedMobile
    with CanMorph with NormalGroundDamage
    with FastAttackGround with PermaCloak
