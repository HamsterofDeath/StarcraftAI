package pony
package brain

trait FerrySupport[T <: GroundUnit] extends JobOrSubJob[T] {

  override def higherPriorityOrder: Seq[UnitOrder] = {
    val where = unit.currentTile
    val target = ferryDropTarget

    val myOrder = target.map { to =>
      val needsFerry = !unit.currentArea.exists(_.free(to))
      if (needsFerry) {
        ferryManager.requestFerry_!(unit, to) match {
          case Some(plan) if unit.onGround =>
            Orders.BoardFerry(unit, plan.ferry).toList
          case _ if unit.loaded =>
            // do nothing while in transporter
            Orders.NoUpdate(unit).toList
          case None =>
            //go to some hopefully near point and wait for ferry
            Orders.MoveToTile(unit, to).toList
        }
      } else Nil
    }.getOrElse(Nil)
    if (myOrder.isEmpty) super.higherPriorityOrder else myOrder
  }

  protected def ferryDropTarget: Option[MapTilePosition]
}
