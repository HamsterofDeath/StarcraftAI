package pony

trait HasMana extends WrapsUnit {
  def mana = nativeUnit.getEnergy
}
