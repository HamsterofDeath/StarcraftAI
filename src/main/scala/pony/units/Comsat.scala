package pony
package units

import bwapi.{Unit => APIUnit, _}
import pony.tech.Upgrades.Terran.ScannerSweep
import pony.tech.Upgrades.SinglePointMagicSpell

class Comsat(unit: APIUnit)
    extends AnyUnit(unit) with AreaSpellcasterBuilding with Addon with TerranBuilding {
  override type Caster = Comsat
  override val spells: List[SinglePointMagicSpell] = List(ScannerSweep)
}
