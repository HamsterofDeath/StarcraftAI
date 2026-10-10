package pony
package units

trait CanHide extends WrapsUnit {
  def isExposed = isVisible

  def isVisible: Boolean
  final def isHidden = !isVisible
}
