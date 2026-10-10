package pony
package units

import pony.combat.Small

trait IsSmall extends Mobile with CanDie {
  override val armorType = Small
}
