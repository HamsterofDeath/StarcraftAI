package pony

trait SimplePosition extends WrapsUnit {
  override def center = {
    val p = nativeUnit.getPosition
    MapPosition(p.getX, p.getY)
  }
}
