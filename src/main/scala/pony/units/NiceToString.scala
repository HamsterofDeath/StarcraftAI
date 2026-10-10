package pony
package units

trait NiceToString extends WrapsUnit {
  override def toString = s"[$unitIdText] ${getClass.className}"
}
