package pony
package units

trait VirtualCloak extends CanCloak with Virtual {

  case class CloakStateSnapshot(cloaked: Boolean)

  private var lastSeen   = Option.empty[CloakStateSnapshot]
  override def isCloaked = lastSeen.map(_.cloaked).getOrElse(super.isCloaked)

  override def remember_!() = {
    super.remember_!()
    if (isEnemy) {
      lastSeen = CloakStateSnapshot(isCloaked).toSome
    }
  }

  override def forget_!() = {
    super.forget_!()
    lastSeen = None
  }
}
