package pony
package brain
package modules

/**
  * "Hit, gain distance, repeat" for a ranged unit facing melee enemies, and "fly in, fire, go back" against static
  * defence. Pure, so it can be tested without a game; all positions and distances are pixels, speeds are pixels per
  * frame and the cooldown is in frames.
  */
object KitingPolicy {

  final case class Point(x: Double, y: Double) {
    def distanceTo(other: Point): Double                = math.hypot(x - other.x, y - other.y)
    def towards(angle: Double, distance: Double): Point =
      Point(x + math.cos(angle) * distance, y + math.sin(angle) * distance)
  }

  /**
    * An enemy the shooter can attack. `reach` is the centre distance at which it can hit the shooter (0 when it
    * cannot attack the shooter at all); only enemies the shooter outranges are worth stepping away from. A threat with
    * speed 0 is static defence; `aimsAtMe` means it is attacking this shooter.
    */
  final case class Threat(
      id: Int,
      at: Point,
      durability: Int,
      reach: Double,
      speed: Double,
      aimsAtMe: Boolean = false,
      ground: Boolean = false,
      focus: FocusFacts = FocusFacts()
  )

  /**
    * What the focus-fire modes weigh, from the shooter's point of view; zeros where unknown. `shots`: shots it needs to
    * kill the enemy, shields taking full damage and hit points the damage its type and the enemy's size and armor let
    * through; `shotDamage`: what its next shot takes off; `dps`: damage per frame the enemy deals to it; `worth`: the
    * enemy's value, casters with energy and cloakers raised, spent casters lowered.
    */
  final case class FocusFacts(shots: Double = 0, shotDamage: Double = 0, dps: Double = 0, worth: Double = 0)

  /**
    * How a ready weapon picks among enemies in range: the one with the least durability left (the old rule), the one
    * killed in the fewest shots (most kills), the one dealing the most damage per shot it takes to kill, nearer ones
    * counting more (least damage taken), or the one taking the most damage per shot (most damage dealt).
    */
  enum FocusMode {
    case Weakest, Kills, Threat, Damage
  }

  object FocusMode {
    def parse(name: String): FocusMode = values.find(_.toString.equalsIgnoreCase(name)).getOrElse(Weakest)
  }

  /** `range` is the centre distance at which the shooter's shot lands; `firing` means its attack is under way. */
  final case class Shooter(
      at: Point,
      range: Double,
      cooldown: Int,
      firing: Boolean,
      speed: Double,
      flying: Boolean = false
  )

  sealed trait Decision

  /** No melee pressure: other behaviours may command the unit. */
  case object Free extends Decision

  /** Leave the current command alone, because the shot is being fired or the unit is safe while it reloads. */
  case object Hold extends Decision

  final case class Shoot(targetId: Int) extends Decision

  final case class Retreat(to: Point) extends Decision

  /** Move closer to the focus target while reloading, because nothing can hit the shooter before its next shot. */
  final case class Approach(to: Point) extends Decision

  /** Who leaves the reach of static defence while reloading: nobody, the unit it aims at, or every unit. */
  enum Dance {
    case Off, Aimed, All
  }

  /** Frames of extra distance kept so a threat that turns towards the shooter cannot land a hit. */
  val SafetyFrames = 6

  val Directions = 16

  /**
    * @param lead       frames before the reload ends at which a shooter with a target in range already attacks, so it
    *                   has turned and braked by the time the weapon is ready
    * @param committed  damage other shooters have already committed to each enemy id this frame
    * @param speedRatio how much faster than an enemy the shooter must be before running from it pays off
    * @param dance      which reloading units leave the reach of static defence
    * @param groundWalkable where ground units can stand: a flyer reloads above the rest, where they cannot follow
    */
  def decide(
      me: Shooter,
      threats: Seq[Threat],
      walkable: Point => Boolean,
      step: Double = 96,
      lead: Int = 4,
      committed: Map[Int, Double] = Map.empty,
      speedRatio: Double = 1.25,
      dance: Dance = Dance.Aimed,
      groundWalkable: Point => Boolean = _ => true,
      mode: FocusMode = FocusMode.Weakest
  ): Decision = {
    val runFrom = outranged(me, threats, speedRatio)
    val melee   = threats.filter(t => t.reach > 0 && t.reach <= MeleeReach && !runFrom.contains(t))
    // static defence that hits the shooter wherever the shooter can hit it
    val turrets       = threats.filter(t => t.speed == 0 && t.reach > 0 && t.reach >= me.range)
    lazy val focus    = target(me, threats, committed, mode)
    def arrival       = math.max(0.0, me.at.distanceTo(focus.at) - me.range) / math.max(me.speed, 0.1)
    def insideTurrets = turrets.exists(t => gap(me.at, t) < StandOff)
    // ground units that can hit a flyer and would reach it before its reload ends
    def chasers = threats.filter(t =>
      t.ground && t.reach > 0 && t.speed > 0 &&
        gap(me.at, t) <= t.speed * (me.cooldown + SafetyFrames)
    )
    lazy val overCliff =
      if (me.flying) cliffRetreat(me, chasers, walkable, groundWalkable, step) else None
    if (threats.isEmpty) Free
    // the attack animation runs until the shot is released; once the reload has started the unit may move again
    else if (me.firing && me.cooldown == 0) Hold
    else if (me.cooldown == 0) Shoot(target(me, threats, committed, mode).id)
    else if (me.cooldown <= lead && threats.exists(t => me.at.distanceTo(t.at) <= me.range))
      Shoot(target(me, threats, committed, mode).id)
    // static defence: come back from outside its reach just in time to fire on arrival
    else if (focus.speed == 0 && focus.reach >= me.range && me.cooldown <= lead + arrival) Shoot(focus.id)
    // fire, go back: the unit it shoots at leaves its reach, so it has to switch to another unit
    else if (insideTurrets && (dance == Dance.All || dance == Dance.Aimed && turrets.exists(_.aimsAtMe)))
      retreatPoint(me, turrets, walkable, step).map(Retreat(_)).getOrElse(Hold)
    // fire, go back over a cliff: ground enemies have to go around while the flyer reloads above unwalkable ground
    else if (overCliff.isDefined) Retreat(overCliff.get)
    // kite: clearly faster and longer-ranged, so stepping out of reach costs nothing
    else if (runFrom.exists(t => gap(me.at, t) <= t.speed * (me.cooldown + SafetyFrames))) {
      retreatPoint(
        me,
        runFrom,
        walkable,
        step
      ).map(Retreat(_)).getOrElse(Shoot(target(me, threats, committed, mode).id))
    }
    // maximum damage: too slow to kite, so only the unit in melee contact gives way and drags its attacker along
    else if (melee.exists(t => gap(me.at, t) <= ContactSlack))
      retreatPoint(me, melee, walkable, step / 2).map(Retreat(_)).getOrElse(Hold)
    // the targets cannot fight back: use the reload to stay in range of the focus target
    else if (threats.forall(_.reach == 0)) approach(me, focus).map(Approach(_)).getOrElse(Hold)
    else Hold
  }

  /** Pixels a unit keeps between itself and the reach of static defence while it stays out. */
  val StandOff = 24.0

  /** Centre-distance reach up to which an enemy counts as melee. */
  val MeleeReach = 64.0

  /** An enemy this close to its reach is in contact. */
  val ContactSlack = 8.0

  /** Fraction of its range a shooter keeps to the focus target while reloading. */
  val ApproachFraction = 0.85

  /** A point at a comfortable fraction of the shooter's range from the target, if the shooter is farther away. */
  def approach(me: Shooter, focus: Threat): Option[Point] = {
    val desired  = me.range * ApproachFraction
    val distance = me.at.distanceTo(focus.at)
    if (distance <= desired + 16) None
    else {
      val scale = desired / distance
      Some(Point(focus.at.x + (me.at.x - focus.at.x) * scale, focus.at.y + (me.at.y - focus.at.y) * scale))
    }
  }

  /**
    * Focus fire without overkill: among enemies in range, the one with the least durability left after the damage other
    * shooters already committed this frame, skipping enemies whose committed damage already kills them; without an
    * enemy in range, the closest one.
    */
  def target(
      me: Shooter,
      threats: Seq[Threat],
      committed: Map[Int, Double] = Map.empty,
      mode: FocusMode = FocusMode.Weakest
  ): Threat = {
    val inRange         = threats.filter(t => me.at.distanceTo(t.at) <= me.range)
    def left(t: Threat) = t.durability - committed.getOrElse(t.id, 0.0)
    if (inRange.nonEmpty) {
      val notDoomed = inRange.filter(left(_) > 0)
      val pool      = if (notDoomed.nonEmpty) notDoomed else inRange
      // shots still needed once the damage already committed this frame lands
      def shotsLeft(t: Threat) =
        if (t.focus.shots <= 0) left(t) else math.max(0.5, t.focus.shots * left(t) / math.max(1, t.durability))
      def distance(t: Threat)  = me.at.distanceTo(t.at)
      def closeness(t: Threat) = 1.0 / (1.0 + math.max(0.0, distance(t) - t.reach) / 64.0)
      mode match {
        case FocusMode.Weakest => pool.minBy(t => (left(t), distance(t)))
        case FocusMode.Kills   => pool.minBy(t => (shotsLeft(t), distance(t)))
        case FocusMode.Threat  =>
          pool.maxBy(t => ((t.focus.dps * closeness(t) * 1000 + t.focus.worth) / shotsLeft(t), -distance(t)))
        case FocusMode.Damage => pool.maxBy(t => (t.focus.shotDamage, -shotsLeft(t)))
      }
    } else threats.minBy(t => me.at.distanceTo(t.at))
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

  /**
    * For a flyer: the point one `step` away above ground nobody walks on that gains the most distance from the ground
    * chasers, if any gains distance at all.
    */
  def cliffRetreat(
      me: Shooter,
      chasers: Seq[Threat],
      passable: Point => Boolean,
      groundWalkable: Point => Boolean,
      step: Double
  ): Option[Point] =
    if (chasers.isEmpty) None
    else {
      val now = chasers.map(t => gap(me.at, t)).min
      (0 until Directions).iterator.map(i => me.at.towards(2 * math.Pi * i / Directions, step))
        .filter(p => passable(p) && !groundWalkable(p))
        .map(p => p -> chasers.map(t => gap(p, t)).min)
        .filter(_._2 > now)
        .maxByOption(_._2)
        .map(_._1)
    }

  private def gap(at: Point, threat: Threat) = at.distanceTo(threat.at) - threat.reach

  /** Enemies that can hit the shooter, reach less far and are clearly slower: running from them gains free shots. */
  private def outranged(me: Shooter, threats: Seq[Threat], speedRatio: Double) =
    threats.filter(t => t.reach > 0 && t.reach < me.range && t.speed * speedRatio <= me.speed)

  /** Each blocked probe around the point costs a step: dead ends score worse than open ground. */
  private def crampedPenalty(p: Point, walkable: Point => Boolean, step: Double) = (0 until 8).count(i =>
    !walkable(p.towards(2 * math.Pi * i / 8, step * 1.5))
  ) * step / 2
}
