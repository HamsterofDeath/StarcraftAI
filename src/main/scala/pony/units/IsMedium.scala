package pony
package units

import pony.combat.Medium

trait IsMedium extends Mobile with CanDie {
  override val armorType = Medium
}
