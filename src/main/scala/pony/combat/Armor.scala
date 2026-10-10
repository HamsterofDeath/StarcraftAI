package pony
package combat

import pony.units.CanDie

case class Armor(armorType: ArmorType, hp: HitPoints, armor: Int, owner: CanDie)
