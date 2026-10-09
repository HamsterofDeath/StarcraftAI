package pony
package brain
package modules

import scala.reflect.ClassTag

class StopMechanic(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Lockdown)
