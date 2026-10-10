package pony
package units

trait HasMana extends WrapsUnit {
  def mana = nativeUnit.getEnergy
}
