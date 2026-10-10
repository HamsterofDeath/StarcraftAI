package pony
package brain
package jobs

import pony.units.WrapsUnit

trait IssueOrderNTimes[T <: WrapsUnit] extends UnitWithJob[T] {
  private var issued                        = 0
  protected def resetIssuedOrders_!(): Unit = { issued = 0 }
  def getOrder: Seq[UnitOrder]
  override def ordersForTick: Seq[UnitOrder] = {
    if (issued == times)
      Nil
    else {
      issued += 1
      getOrder
    }
  }
  def times = 1
}
