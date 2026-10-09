package pony
package brain

trait FerrySupport[T <: GroundUnit] extends JobOrSubJob[T] {

  override def higherPriorityOrder: Seq[UnitOrder] = {
    val where  = unit.currentTile
    val target = ferryDropTarget

    val myOrder = target.map { to =>
      val needsFerry = !unit.currentArea.exists(_.free(to)) || ferryManager.sealedApart(where, to)
      ferryWait = needsFerry
      if (needsFerry) {
        ferryManager.requestFerry_!(unit, to) match {
          case Some(plan) if unit.onGround =>
            Orders.BoardFerry(unit, plan.ferry).toList
          case _ if unit.loaded =>
            // do nothing while in transporter
            Orders.NoUpdate(unit).toList
          case None =>
            // go to some hopefully near point and wait for ferry
            Orders.MoveToTile(unit, to).toList
          case Some(_) =>
            // neither grounded nor loaded: boarding is in progress, so leave the unit alone
            Orders.NoUpdate(unit).toList
        }
      } else Nil
    }.getOrElse(Nil)
    if (target.isEmpty) ferryWait = false
    if (myOrder.isEmpty) super.higherPriorityOrder else myOrder
  }

  private var ferryWait = false

  /** Whether the unit's way to its target needs a ferry, as of its last order. */
  def waitsForFerry = ferryWait

  protected def ferryDropTarget: Option[MapTilePosition]
}
