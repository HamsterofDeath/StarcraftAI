package pony
package brain
package modules

import scala.collection.mutable

class DefaultBehaviours(universe: Universe) extends OrderlessAIModule[WrapsUnit](universe) {
  self =>
  private val rules  = TerranBehaviours.allBehaviours(universe)
  private val ignore = mutable.HashSet.empty[WrapsUnit]

  override def renderDebug(renderer: Renderer): Unit = {
    rules.foreach(_.renderDebug_!(renderer))
  }

  override def onTick_!(): Unit = {
    rules.foreach(_.onTick_!())
    // fetch all idles and assign "always on" background tasks to them
    val hireUs = unitManager.allIdles.filterNot(e => ignore(e.unit)).flatMap { free =>
      val unit = free.unit
      rules.filter(_.canControl(unit)).foreach(_.add_!(unit, Objective.initial))
      val behaviours = rules.filter(_.controls(unit)).flatMap(_.behaviourOf(unit))
      if (behaviours.nonEmpty) {
        Some(new BusyDoingSomething(self, behaviours, Objective.initial))
      } else {
        ignore += unit
        None
      }
    }
    info(s"Attaching default behaviour to new ${hireUs.size} units", hireUs.nonEmpty)
    hireUs.foreach { unitManager.assignJob_! }
  }
}
