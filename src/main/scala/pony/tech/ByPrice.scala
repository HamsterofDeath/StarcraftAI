package pony
package tech

import pony.units.Mobile

import pony.tech.Upgrades.IsTech

trait ByPrice extends IsTech {
  override def priorityRule: Option[(Mobile) => Double] = Some(m => m.price.sum)
}
