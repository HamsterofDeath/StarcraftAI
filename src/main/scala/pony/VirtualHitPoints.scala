package pony

trait VirtualHitPoints extends CanDie with Virtual {

  case class HitpointsSnapshot(hitPoints: HitPoints)

  private var lastSeen = Option.empty[HitpointsSnapshot]

  override def hitPoints = lastSeen.map(_.hitPoints).getOrElse(super.hitPoints)

  override def remember_!() = {
    super.remember_!()
    if (isEnemy) {
      lastSeen = HitpointsSnapshot(hitPoints).toSome
    }
  }

  override def forget_!() = {
    super.forget_!()
    lastSeen = None
  }

}
