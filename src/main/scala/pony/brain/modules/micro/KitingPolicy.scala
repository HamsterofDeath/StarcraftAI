package pony
package brain
package modules
package micro

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
    def parse(name: String): FocusMode = values.find(_.toString.equalsIgnoreCase(name)).getOrElse(Threat)
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
    * @param repairRally  where repair waits for this hurt shooter: it leaves static defence towards that point and
    *                     stays out until mended instead of returning the moment its weapon is ready again
    * @param leaveNow     this pass against static defence is spent: leave its reach even with the weapon ready
    * @param groupReady   the rest of the flight is ready to go in; while not, hold the standoff outside the reach
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
      meleeStandoff: Double = ContactSlack,
      fallBackWhenTargeted: Boolean = false,
      rally: Option[Point] = None,
      repairRally: Option[Point] = None,
      leaveNow: Boolean = false,
      groupReady: Boolean = true,
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
    // mobile ranged enemies that can hit this shooter before its reload ends
    def rangedChasers = threats.filter(t =>
      t.speed > 0 && t.reach > MeleeReach &&
        gap(me.at, t) <= t.speed * (me.cooldown + SafetyFrames)
    )
    lazy val overCliff =
      if (me.flying) cliffRetreat(me, chasers, walkable, groundWalkable, step) else None
    if (threats.isEmpty) Free
    // the attack animation runs until the shot is released; once the reload has started the unit may move again
    else if (me.firing && me.cooldown == 0) Hold
    // hurt, with repair waiting outside: stay out and mend, even with the weapon ready — returning on reload
    // alone accumulates damage faster than the SCVs can take it off between shots
    else if (repairRally.isDefined && turrets.nonEmpty && !insideTurrets && me.cooldown <= lead)
      if (me.at.distanceTo(repairRally.get) > CohesionRadius) Approach(repairRally.get) else Hold
    // the pass is spent (shots fired, or this shooter is the one under fire): leave even with the weapon ready
    else if (leaveNow && insideTurrets)
      retreatPoint(me, turrets, walkable, step, repairRally.orElse(rally)).map(Retreat(_)).getOrElse(Hold)
    // the rest of the flight is not ready: hold the standoff outside the reach instead of going in alone
    else if (!groupReady && repairRally.isEmpty && turrets.nonEmpty && !insideTurrets)
      standoff(me, focus).map(Approach(_)).getOrElse(Hold)
    else if (me.cooldown == 0) Shoot(target(me, threats, committed, mode).id)
    else if (me.cooldown <= lead && threats.exists(t => me.at.distanceTo(t.at) <= me.range))
      Shoot(target(me, threats, committed, mode).id)
    // static defence: come back from outside its reach just in time to fire on arrival; from inside there is
    // nothing to come back to — the way out comes first, or the shooter never leaves the reach at all
    else if (
      repairRally.isEmpty && !insideTurrets && focus.speed == 0 && focus.reach >= me.range &&
      me.cooldown <= lead + arrival
    ) Shoot(focus.id)
    // fire, go back: the unit it shoots at leaves its reach, so it has to switch to another unit; a hurt
    // shooter with repair waiting always leaves, towards the repair point
    else if (
      insideTurrets &&
      (repairRally.isDefined || dance == Dance.All ||
        dance == Dance.Aimed && turrets.exists(_.aimsAtMe))
    )
      retreatPoint(me, turrets, walkable, step, repairRally.orElse(rally)).map(Retreat(_)).getOrElse(Hold)
    // hurt and reloading outside the reach, with repair waiting: spend the reload flying to it
    else if (repairRally.isDefined && turrets.nonEmpty)
      if (me.at.distanceTo(repairRally.get) > CohesionRadius) Approach(repairRally.get) else Hold
    // fire, go back over a cliff: ground enemies have to go around while the flyer reloads above unwalkable ground
    else if (overCliff.isDefined) Retreat(overCliff.get)
    // fire, fall back: the shooter the ranged enemies are firing at gives ground while it reloads; they must
    // stop to shoot and then catch up again, and the next shooter in line takes their fire instead
    else if (fallBackWhenTargeted && rangedChasers.nonEmpty)
      retreatPoint(me, rangedChasers, walkable, step, rally).map(Retreat(_)).getOrElse(Hold)
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
    else if (melee.exists(t => gap(me.at, t) <= meleeStandoff))
      retreatPoint(me, melee, walkable, if (meleeStandoff > ContactSlack) step else step / 2, rally)
        .map(Retreat(_)).getOrElse(Hold)
    // the targets cannot fight back: use the reload to stay in range of the focus target
    else if (threats.forall(_.reach == 0)) approach(me, focus).map(Approach(_)).getOrElse(Hold)
    // annoy: something can be hit from where nothing hits back before the next shot: spend the reload getting there
    else annoyPoint(me, threats, walkable).map(Approach(_)).getOrElse(Hold)
    // (annoyPoint already falls back to Hold where no free shot exists)
  }

  /** Under threat focus fire, an enemy this few shots from death counts this much more: kills end its damage. */
  val FinishingShots = 2.0
  val FinishingBonus = 2.0

  /** Under threat focus fire, enemies within this many pixels of hitting the shooter all count as equally near. */
  val NearSlack = 96.0

  /** Pixels a unit keeps between itself and the reach of static defence while it stays out. */
  val StandOff = 24.0

  /** Centre-distance reach up to which an enemy counts as melee. */
  val MeleeReach = 64.0

  /** An enemy this close to its reach is in contact. */
  val ContactSlack = 8.0

  /** A rally point this many pixels away still counts as staying with the group. */
  val CohesionRadius = 64.0

  /** How much each pixel beyond the cohesion radius costs a retreat point, against the distance it gains. */
  val CohesionWeight = 0.5

  /** Fraction of its range a shooter keeps to the focus target while reloading. */
  val ApproachFraction = 0.85

  /** A point at a comfortable fraction of the shooter's range from the target, if the shooter is farther away. */
  /** Pixels of slack inside its own range and outside an enemy's approach a free shot keeps. */
  val AnnoySlack = 16.0

  /**
    * Annoy: a walkable point inside the shooter's range of an enemy it outranges, far enough out that no enemy can
    * hit the shooter there before its next shot — free damage, however little, mostly for long-range ground units
    * and flyers over enemies that cannot reach them. Nothing when the shooter cannot move (a sieged tank already
    * sits where it sits), when something is already in range to shoot at, or when an enemy's approach during the
    * reload covers the whole band inside the shooter's range.
    */
  def annoyPoint(me: Shooter, threats: Seq[Threat], walkable: Point => Boolean): Option[Point] =
    if (me.speed <= 0 || threats.exists(t => me.at.distanceTo(t.at) <= me.range)) None
    else {
      val victims = threats.filter(t => t.reach < me.range).sortBy(t => me.at.distanceTo(t.at))
      victims.iterator.flatMap { victim =>
        // the closest the shooter may stand to the victim and still not be hit before its next shot
        val keepOut = victim.reach + victim.speed * (me.cooldown + SafetyFrames)
        val desired = math.max(me.range * ApproachFraction, keepOut + AnnoySlack)
        if (desired > me.range - AnnoySlack) None
        else {
          val distance = me.at.distanceTo(victim.at)
          val scale    = desired / distance
          val p        = Point(
            victim.at.x + (me.at.x - victim.at.x) * scale,
            victim.at.y + (me.at.y - victim.at.y) * scale
          )
          if (walkable(p) && threats.forall(t => gap(p, t) > t.speed * (me.cooldown + SafetyFrames))) Some(p)
          else None
        }
      }.nextOption()
    }

  /**
    * How many times the flight's damage per frame must beat the static defence's before only the targeted unit
    * steps out (Dance.Aimed) and the rest keep firing; below it the flight goes in and out as a group (Dance.All).
    */
  val OverwhelmRatio = 3.0

  /** Whether `allies` shooters of `myDps` each overwhelm static defence answering with `theirDps` in total. */
  def overwhelms(allies: Int, myDps: Double, theirDps: Double, ratio: Double = OverwhelmRatio): Boolean =
    theirDps <= 0 || allies * myDps >= ratio * theirDps

  /** A point just outside the focus enemy's reach on the way to it, if the shooter is farther out than that. */
  def standoff(me: Shooter, focus: Threat): Option[Point] = {
    val desired  = focus.reach + StandOff
    val distance = me.at.distanceTo(focus.at)
    if (distance <= desired + 16) None
    else {
      val scale = desired / distance
      Some(Point(focus.at.x + (me.at.x - focus.at.x) * scale, focus.at.y + (me.at.y - focus.at.y) * scale))
    }
  }

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
    * enemy in range, the closest one — except under weakest focus, where the weakest overall keeps a whole
    * flight burning the same static target down instead of splitting over shields that regrow.
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
      def distance(t: Threat) = me.at.distanceTo(t.at)
      // nearness counts, but less than finishing, and everything about to hit counts as near: in mixed armies each
      // shooter ranked its own neighbour first, so the group spread its damage instead of finishing units
      def closeness(t: Threat) = 1.0 / (1.0 + math.max(0.0, distance(t) - t.reach - NearSlack) / 128.0)
      def finishing(t: Threat) = if (shotsLeft(t) <= FinishingShots) FinishingBonus else 1.0
      mode match {
        case FocusMode.Weakest => pool.minBy(t => (left(t), distance(t)))
        case FocusMode.Kills   => pool.minBy(t => (shotsLeft(t), distance(t)))
        case FocusMode.Threat  =>
          pool.maxBy(t =>
            ((t.focus.dps * closeness(t) * 1000 + t.focus.worth) * finishing(t) / shotsLeft(t), -distance(t))
          )
        case FocusMode.Damage => pool.maxBy(t => (t.focus.shotDamage, -shotsLeft(t)))
      }
    } else if (mode == FocusMode.Weakest) threats.minBy(t => (left(t), me.at.distanceTo(t.at)))
    else threats.minBy(t => me.at.distanceTo(t.at))
  }

  /**
    * The walkable point one `step` away that keeps the largest distance to the nearest threat, preferring open ground so
    * the shooter does not run itself into a wall or a corner.
    */
  def retreatPoint(
      me: Shooter,
      threats: Seq[Threat],
      walkable: Point => Boolean,
      step: Double,
      rally: Option[Point] = None
  ): Option[Point] = {
    (0 until Directions).iterator.map(i => me.at.towards(2 * math.Pi * i / Directions, step))
      .filter(p => walkable(p) && walkable(Point((p.x + me.at.x) / 2, (p.y + me.at.y) / 2)))
      .map { p =>
        val cohesion = rally.fold(0.0)(r => math.max(0.0, p.distanceTo(r) - CohesionRadius) * CohesionWeight)
        p -> (threats.map(t => gap(p, t)).min - crampedPenalty(p, walkable, step) - cohesion)
      }
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
