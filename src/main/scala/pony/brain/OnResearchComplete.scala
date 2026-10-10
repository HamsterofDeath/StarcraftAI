package pony
package brain

import pony.tech.Upgrade

trait OnResearchComplete {
  def onComplete(upgrade: Upgrade): Unit
}
