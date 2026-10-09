package pony

object Price {
  val zero                          = Price(0, 0)
  implicit val ord: Ordering[Price] = Ordering.fromLessThan[Price](_ < _)
}

case class Price(minerals: Int, gas: Int) {
  val sum = minerals + gas

  def -(other: Price) = Price(minerals - other.minerals, gas - other.gas)

  def <(other: Price) = sum < other.sum

  def +(price: Price) = Price(minerals + price.minerals, gas + price.gas)
}
