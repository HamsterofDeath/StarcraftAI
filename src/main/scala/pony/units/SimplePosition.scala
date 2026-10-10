package pony
package units

import pony.geometry.MapPosition

trait SimplePosition extends WrapsUnit {
  override def center = {
    val p = nativeUnit.getPosition
    MapPosition(p.getX, p.getY)
  }
}
