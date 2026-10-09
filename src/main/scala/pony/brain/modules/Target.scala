package pony
package brain
package modules

case class Target[T <: Mobile](caster: HasSingleTargetSpells, target: T)
