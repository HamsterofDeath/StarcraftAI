package pony
package units

import bwapi.{Unit => APIUnit, _}

class Pylon(unit: APIUnit)
    extends AnyUnit(unit) with Building with PsiArea with ImmobileSupplyProvider
