package pony
package brain
package modules
package economy

import pony.brain.jobs.UnitWithJob
import pony.units.{MineralPatch, WorkerUnit}

// need this for instanceof checks
trait GatherMineralsAtSinglePatch extends UnitWithJob[WorkerUnit] {
  def worker: WorkerUnit
  def targetPatch: MineralPatch
  def requiredWorkers: Int
}
