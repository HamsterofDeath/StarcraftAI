package pony

trait CanHide extends WrapsUnit {
  def isExposed = isVisible

  def isVisible: Boolean
  final def isHidden = !isVisible
}
