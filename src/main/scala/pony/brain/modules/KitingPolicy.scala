package pony
package brain
package modules

/**
  * "Hit, gain distance, repeat" for a ranged unit facing melee enemies. Pure, so it can be tested without a game; all
  * positions and distances are pixels, speeds are pixels per frame and the cooldown is in frames.
  */
object KitingPolicy {

  final case class Point(x: Double, y: Double) {
    def distanceTo(other: Point): Double                = math.hypot(x - other.x, y - other.y)
    def towards(angle: Double, distance: Double): Point =
      Point(x + math.cos(angle) * distance, y + math.sin(angle) * distance)
  }

  /**
    * An enemy the shooter can attack. `reach` is the centre distance at which it can hit the shooter (0 when it
    * cannot attack the shooter at all); only enemies the shooter outranges are worth stepping away from.
    */
  final case class Threat(id: Int, at: Point, durability: Int, reach: Double, speed: Double)

  /** `range` is the centre distance at which the shooter's shot lands; `firing` means its attack is under way. */
  final case class Shooter(at: Point, range: Double, cooldown: Int, firing: Boolean, speed: Double)

  sealed trait Decision

  /** No melee pressure: other behaviours may command the unit. */
  case object Free extends Decision

  /** Leave the current command alone, because the shot is being fired or the unit is safe while it reloads. */
  case object Hold extends Decision

  final case class Shoot(targetId: Int) extends Decision

  final case class Retreat(to: Point) extends Decision

  /** Frames of extra distance kept so a threat that turns towards the shooter cannot land a hit. */
  val SafetyFrames = 6

  val Directions = 16

  /**
    * @param lead frames before the reload ends at which a shooter with a target in range already attacks, so it has
    *             turned and braked by the time the weapon is ready
    */
  def decide(
      me: Shooter,
      threats: Seq[Threat],
      walkable: Point => Boolean,
      step: Double = 96,
      lead: Int = 4
  ): Decision = {
    if (threats.isEmpty) Free
    // the attack animation runs until the shot is released; once the reload has started the unit may move again
    else if (me.firing && me.cooldown == 0) Hold
    else if (me.cooldown == 0) Shoot(target(me, threats).id)
    else if (me.cooldown <= lead && threats.exists(t => me.at.distanceTo(t.at) <= me.range))
      Shoot(target(me, threats).id)
    else if (outranged(me, threats).exists(t => gap(me.at, t) <= t.speed * (me.cooldown + SafetyFrames))) {
      retreatPoint(me, outranged(me, threats), walkable, step).map(Retreat(_))
        .getOrElse(Shoot(target(me, threats).id))
    } else Hold
  }

  /** Focus fire: the weakest enemy in range, the closest one breaking ties; without one in range, the closest. */
  def target(me: Shooter, threats: Seq[Threat]): Threat = {
    val inRange = threats.filter(t => me.at.distanceTo(t.at) <= me.range)
    if (inRange.nonEmpty) inRange.minBy(t => (t.durability, me.at.distanceTo(t.at)))
    else threats.minBy(t => me.at.distanceTo(t.at))
  }

  /**
    * The walkable point one `step` away that keeps the largest distance to the nearest threat, preferring open ground so
    * the shooter does not run itself into a wall or a corner.
    */
  def retreatPoint(me: Shooter, threats: Seq[Threat], walkable: Point => Boolean, step: Double): Option[Point] = {
    (0 until Directions).iterator.map(i => me.at.towards(2 * math.Pi * i / Directions, step))
      .filter(p => walkable(p) && walkable(Point((p.x + me.at.x) / 2, (p.y + me.at.y) / 2)))
      .map(p => p -> (threats.map(t => gap(p, t)).min - crampedPenalty(p, walkable, step)))
      .maxByOption(_._2)
      .map(_._1)
  }

  private def gap(at: Point, threat: Threat) = at.distanceTo(threat.at) - threat.reach

  /** Enemies that can hit the shooter but have less reach: running from them gains free shots. */
  private def outranged(me: Shooter, threats: Seq[Threat]) = threats.filter(t => t.reach > 0 && t.reach < me.range)

  /** Each blocked probe around the point costs a step: dead ends score worse than open ground. */
  private def crampedPenalty(p: Point, walkable: Point => Boolean, step: Double) = (0 until 8).count(i =>
    !walkable(p.towards(2 * math.Pi * i / 8, step * 1.5))
  ) * step / 2
}
