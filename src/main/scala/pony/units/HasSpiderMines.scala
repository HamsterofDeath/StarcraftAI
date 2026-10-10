package pony
package units

trait HasSpiderMines extends WrapsUnit {
  private val mines   = oncePerTick { nativeUnit.getSpiderMineCount }
  def spiderMineCount = mines.get
}
