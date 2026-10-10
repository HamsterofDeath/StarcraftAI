package pony
package units

trait Virtual extends CanDie {

  def remember_!(): Unit = {}

  def forget_!(): Unit = {}

  def update_!(): Unit = {
    forget_!()
    remember_!()
  }

}
