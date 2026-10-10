package pony
package brain
package jobs

import pony.brain.budget.ResourceApprovalFail
import pony.brain.requests.{BuildUnitRequest, UnitJobRequest}
import pony.units.{AutoPilot, WrapsUnit}

import pony.brain.modules.production.AlternativeBuildingSpot

import scala.collection.mutable
import scala.reflect.ClassTag

class UnitCollector[T <: WrapsUnit: ClassTag](req: UnitJobRequest[T], override val universe: Universe)
    extends HasUniverse {

  private val hired              = mutable.HashSet.empty[T]
  private var remainingOpenSpots = req.request.amount

  def onlyMember = {
    assert(hasOneMember)
    hired.head
  }

  def hasOneMember = hired.size == 1

  def collect_!(includeCandidates: Set[UnitWithJob[T]] = Set
    .empty[UnitWithJob[T]]): Option[UnitCollector[T]] = {
    val um        = unitManager
    val available = {
      val potential = {
        def allWithType = um.allOfEmployerAndType(um.Nobody, req.requestedUnitType).iterator ++
          um.allNotOfEmployerButType(um.Nobody, req.requestedUnitType)

        val stage0 = allWithType.toVector
        val stage1 = stage0.filter(_.unit.isInGame)
        val stage2 = stage1.filter {
          _.unit match {
            case a: AutoPilot => a.isManuallyControlled
            case _            => true
          }
        }
        val stage3 = stage2.filter(_.priority < req.priority)
        val stage4 = stage3.filter(interrupts)
        val stage5 = stage4.filter(requests)
        if (universe.currentTick < 3000 && stage5.isEmpty)
          NativeMatchEvidence.trace(
            "collector-stages",
            s"type=${req.requestedUnitType.getSimpleName} all=${stage0.size} inGame=${stage1.size} auto=${stage2.size} prio=${stage3.size} intr=${stage4.size} wants=${stage5.size} reqPrio=${req.priority} sample=${stage0.take(8).map(
                j => s"#${j.unit.nativeUnitId}:${j.priority}:${j.isIdle}:${j.unit.getClass.getSimpleName}"
              ).mkString(",")}"
          )
        val withExplicitCandidates = stage5 ++ includeCandidates
        withExplicitCandidates.map(typed).toVector
      }
      priorityRule.fold(potential) { rule =>
        val prepared = potential.map(e => e -> rule.giveRating(e))
        prepared.sortBy(_._2).map(_._1)
      }
    }

    val candidates = available.iterator
    var filled     = false
    while (!filled && candidates.hasNext) {
      collect(candidates.next().unit)
      filled = complete
    }

    if (filled || hasAny)
      Some(this)
    else
      None
  }

  def typed(any: UnitWithJob[? <: WrapsUnit]) = any.asInstanceOf[UnitWithJob[T]]

  def hasAny = hired.nonEmpty

  def collect(unit: WrapsUnit) = {
    if (remainingOpenSpots > 0 && req.request.includesByType(unit)) {
      remainingOpenSpots -= 1
      // we know the type because we asked req before
      hired += unit.asInstanceOf[T]
    }
  }

  def complete = remainingOpenSpots == 0

  def requests(unit: UnitWithJob[? <: WrapsUnit]) = {
    req.wantsUnit(unit.unit)
  }

  def interrupts(unit: UnitWithJob[? <: WrapsUnit]) = {
    req.canInterrupt(unit)
  }

  def hasPriorityRule = priorityRule.isDefined

  def priorityRule = req.priorityRule

  def missingAsRequest: UnitJobRequest[T] = {
    val typesAndAmounts =
      BuildUnitRequest[T](
        universe,
        req.request.typeOfRequestedUnit,
        remainingOpenSpots,
        ResourceApprovalFail,
        Priority.Default,
        AlternativeBuildingSpot.useDefault
      )

    UnitJobRequest[T](typesAndAmounts, req.employer, req.priority)
  }

  override def toString: String = s"Collected: $teamAsCanHireInfo"

  def teamAsCanHireInfo = CanHireInfo(Some(req), hired.toSet)
}
