package pony
package units

import bwapi.{Unit => APIUnit, _}
import pony.tech.Upgrades.Terran.Nuke
import pony.tech.Upgrades.SinglePointMagicSpell

class NuclearSilo(unit: APIUnit)
    extends AnyUnit(unit) with AreaSpellcasterBuilding with Addon with TerranBuilding {
  override type Caster = NuclearSilo
  override val spells: List[SinglePointMagicSpell] = List(Nuke)
}
