package pony
package combat

import pony.tech.Upgrade
import pony.units.Mobile

import pony.tech.Upgrades.SingleTargetMagicSpell

import scala.reflect.ClassTag

abstract class SingleTargetSpell[C <: HasSingleTargetSpells, M <: Mobile: ClassTag](val tech: Upgrade &
  SingleTargetMagicSpell) {
  val castRange       = 300
  val castRangeSquare = castRange * castRange

  private val targetClass = tech.canCastOn

  assert(
    targetClass >= implicitly[ClassTag[M]].runtimeClass,
    s"$targetClass vs ${implicitly[ClassTag[M]].runtimeClass}"
  )

  def castOn: CastOn = EnemyUnits

  def shouldActivateOn(validated: M) = true

  def casted(m: Mobile) = {
    assert(canBeCastOn(m))
    m.asInstanceOf[M]
  }

  def canBeCastOn(m: Mobile) = targetClass.isInstance(m)

  def isAffected(m: M): Boolean

  def priorityRule = tech.priorityRule
}
