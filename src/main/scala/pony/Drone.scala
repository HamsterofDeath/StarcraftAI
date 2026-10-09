package pony

import bwapi.{Unit => APIUnit, _}

class Drone(unit: APIUnit)
    extends AnyUnit(unit) with WorkerUnit with IsSmall with Organic with NormalGroundDamage with FastAttackGround
    with ZergMobileUnit
