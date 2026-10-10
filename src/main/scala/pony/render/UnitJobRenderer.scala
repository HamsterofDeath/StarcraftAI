package pony
package render

import pony.units.{AirUnit, Building, GroundUnit, Mobile}

import pony.brain.{HasUniverse, Universe}

class UnitJobRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      val renderUs = unitManager.allJobsByUnitType[Mobile].filter { job =>
        job.unit match {
          case g: GroundUnit if !g.loaded => true
          case a: AirUnit                 => true
          case _                          => false
        }
      }

      renderUs.foreach { job =>
        renderer.drawTextAtMobileUnit(
          job.unit,
          s"${job.shortDebugString} -> ${job.unit.nativeUnit.getOrder}",
          1
        )
        job.renderDebug(renderer)
      }
      unitManager.allJobsByUnitType[Building].foreach { job =>
        renderer.drawTextAtStaticUnit(
          job.unit,
          s"${job.shortDebugString} -> ${job.unit.nativeUnit.getOrder}",
          1
        )
      }
    }
  }

  override def lazyWorld: DefaultWorld = universe.world
}
