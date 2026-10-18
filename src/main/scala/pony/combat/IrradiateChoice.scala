package pony
package combat

/**
  * Which unit an Irradiate should burn, kept free of the game; pixels. The burn deals its whole damage to the target
  * and to every organic unit that stays near it, friend or foe, so the best target is usually the middle of a group.
  * Only when no group is in reach does the single most valuable kill win.
  */
private[pony] object IrradiateChoice {

  /**
    * A unit as the burn sees it: where it stands, the hit points and shields it has and could have, what it cost,
    * whose it is, whether the burn can hurt it at all, and whether it already burns (a second burn adds nothing).
    */
  final case class Blip(
      id: Int,
      x: Double,
      y: Double,
      durability: Double,
      maxDurability: Double,
      price: Double,
      enemy: Boolean,
      organic: Boolean,
      burning: Boolean
  )

  /** The damage one burn deals to each unit it touches, spread over its whole duration, armour ignored. */
  val TotalDamage = 250.0

  /** Below this a burn is not worth its 75 energy; about one zealot killed outright. */
  val MinScore = 200.0

  /** Another vessel leaves a spot just chosen alone this long. */
  val ClaimFrames = 48

  /**
    * What a burn on `centre` is worth: for every unit inside `radius` of it, the damage the burn will do (never more
    * than the unit has left) plus the share of its price that damage destroys — so a kill counts its whole price and
    * a singe only part of it. Our own units count against it, since the burn spares nobody.
    */
  def score(centre: Blip, units: Seq[Blip], radius: Double): Double =
    units.filter(u => math.hypot(u.x - centre.x, u.y - centre.y) <= radius).map { u =>
      val dealt = if (!u.organic || u.burning) 0.0 else math.min(u.durability, TotalDamage)
      val share = if (u.maxDurability <= 0) 0.0 else dealt / u.maxDurability
      val gain  = dealt + u.price * share
      if (u.enemy) gain else -gain
    }.sum

  /** The best centre to burn and its score, ties to the costlier centre; nothing below `MinScore`. */
  def best(centres: Seq[Blip], units: Seq[Blip], radius: Double): Option[(Blip, Double)] =
    centres.map(c => c -> score(c, units, radius)).filter(_._2 >= MinScore)
      .maxByOption { case (c, s) => (s, c.price) }
}
