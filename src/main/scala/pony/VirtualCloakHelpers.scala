package pony

trait VirtualCloakHelpers
    extends CanCloak with Virtual with VirtualCloak with VirtualHitPoints with VirtualPosition {

  override def onTick_!() = {
    super.onTick_!()
    if (isEnemy) {
      if (age == 0) {
        remember_!()
      } else if (isExposed) {
        update_!()
      }
    }
  }
}
