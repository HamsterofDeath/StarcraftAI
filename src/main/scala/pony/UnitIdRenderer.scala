package pony

import bwapi.Color

class UnitIdRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Green)

      val u = lazyWorld.universe
      u.ownUnits.allByType[Mobile]
      .iterator
      .collect {
        case g: GroundUnit if !g.loaded => g
        case a: AirUnit => a
      }
      .foreach { u =>
        renderer.drawTextAtMobileUnit(u, u.shortDebugString)
      }
      u.ownUnits.allByType[Building].foreach { u =>
        renderer.drawTextAtStaticUnit(u, s"${u.shortDebugString}/${u.getClass.className}")
      }
      u.enemyUnits.allByType[Mobile].foreach { u =>
        renderer.drawTextAtMobileUnit(u, u.shortDebugString)
      }
      u.enemyUnits.allByType[Building].foreach { u =>
        renderer.drawTextAtStaticUnit(u, s"${u.shortDebugString}/${u.getClass.className}")
      }
    }
  }
}
