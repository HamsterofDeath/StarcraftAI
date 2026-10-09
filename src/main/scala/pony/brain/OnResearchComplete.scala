package pony
package brain

trait OnResearchComplete {
  def onComplete(upgrade: Upgrade): Unit
}
