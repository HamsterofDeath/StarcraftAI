package pony

import bwapi.{Unit => APIUnit, _}

class SCV(unit: APIUnit)
    extends AnyUnit(unit) with WorkerUnit with IsSmall with NormalGroundDamage with FastAttackGround
