package pony
package tech

import pony.units.{Detector, Mobile}

import pony.tech.Upgrades.IsTech

trait DetectorsFirst extends IsTech {
  override def priorityRule: Option[(Mobile) => Double] = Some { m =>
    m.isInstanceOf[Detector].ifElse(1, 0)
  }
}
