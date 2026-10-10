package pony
package units

import pony.combat.Large

trait IsBig extends Mobile with CanDie {
  override val armorType = Large
}
