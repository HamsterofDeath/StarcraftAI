package pony
package brain.modules

import pony.brain._

object PlanIdCounter {
  private var id = 0

  def nextId() = {
    id += 1
    id
  }

}
