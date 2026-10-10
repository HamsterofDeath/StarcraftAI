package pony
package combat

import bwapi._

sealed class DamageType(native: bwapi.DamageType)

case object Normal extends DamageType(bwapi.DamageType.Normal)

case object Explosive extends DamageType(bwapi.DamageType.Explosive)

case object Concussive extends DamageType(bwapi.DamageType.Concussive)

case object IgnoreArmor extends DamageType(bwapi.DamageType.Ignore_Armor)

case object Independent extends DamageType(bwapi.DamageType.Independent)

case object Unknown extends DamageType(bwapi.DamageType.Unknown)

case object NoDamage extends DamageType(bwapi.DamageType.None)

object DamageTypes {
  def fromNative(dt: bwapi.DamageType) = {
    if (dt == bwapi.DamageType.Concussive) Concussive
    else if (dt == bwapi.DamageType.Explosive) Explosive
    else if (dt == bwapi.DamageType.Normal) Normal
    else if (dt == bwapi.DamageType.Ignore_Armor) IgnoreArmor
    else if (dt == bwapi.DamageType.Independent) Independent
    else if (dt == bwapi.DamageType.None) NoDamage
    else if (dt == bwapi.DamageType.Unknown) Unknown
    else if (dt eq null) !!!("Null?")
    else !!!(s"Unknown damage type :(")
  }
}
