package pony
package brain
package modules
package micro

import pony.combat.Spells

import scala.reflect.ClassTag

class StopMechanic(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Lockdown)
