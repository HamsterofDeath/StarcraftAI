package pony
package brain
package modules

import scala.reflect.ClassTag

class ShieldUnit(universe: Universe)
    extends OneTimeUnitSpellCast(universe, Spells.DefenseMatrix)
