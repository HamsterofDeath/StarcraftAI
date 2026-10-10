package pony
package brain
package jobs

import pony.brain.requests.CanAcceptUnitSwitch
import pony.units.{Controllable, WrapsUnit}

import scala.reflect.ClassTag

class JobReAssignments(universe: Universe) extends OrderlessAIModule[Controllable](universe) {
  override def onTick_!(): Unit = {
    unitManager.nextJobReorganisationRequest
      .filter(_.stillWantsOptimization)
      .foreach { optimizeMe =>
        trace(s"Trying to find better unit for job $optimizeMe")
        if (optimizeMe.hasToSwitchLater) {
          // try again later
          trace(s"Not found, try again later")
          unitManager.tryFindBetterEmployeeFor(optimizeMe)
        } else {
          def doTyped[T <: WrapsUnit: ClassTag](old: CanAcceptUnitSwitch[T]) = {
            val uc = new UnitCollector(old.asRequest, universe).collect_!(Set(old))
            uc match {
              case Some(replacement)
                  if replacement.hasOneMember &&
                    replacement.onlyMember == optimizeMe.unit =>
                trace(s"Same unit chosen as best worker")
              case Some(replacement)
                  if replacement.hasOneMember &&
                    replacement.onlyMember != optimizeMe.unit =>
                info(s"Replacement found: ${replacement.onlyMember}")
                // someone else might have already done this somewhere else
                val nobody = unitManager.Nobody
                if (unitManager.employerOf(optimizeMe.unit).contains(nobody)) {
                  warn(s"Unit ${optimizeMe.unit} was already asigned to $nobody")
                } else {
                  unitManager.assignJob_!(new BusyDoingNothing(optimizeMe.unit, nobody))
                }
                unitManager.assignJob_!(old.copyOfJobForNewUnit(replacement.onlyMember))
              case _ =>
                if (optimizeMe.couldSwitchInTheFuture) {
                  // try again later
                  trace(s"Not found, try again later")
                  unitManager.tryFindBetterEmployeeFor(optimizeMe)
                }
            }
          }
          doTyped(optimizeMe.asInstanceOf[CanAcceptUnitSwitch[WrapsUnit]])
        }
      }
  }
}
