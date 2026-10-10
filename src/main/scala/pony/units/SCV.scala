package pony
package units

import pony.combat.{FastAttackGround, NormalGroundDamage}

import bwapi.{Unit => APIUnit, _}

class SCV(unit: APIUnit)
    extends AnyUnit(unit) with WorkerUnit with IsSmall with NormalGroundDamage with FastAttackGround
