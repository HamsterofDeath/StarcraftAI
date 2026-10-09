package pony
package brain

import java.util.Comparator

case class PriorityChain(data: Vector[Double]) {
  lazy val sum = data.sum
}

object PriorityChain {

  implicit val ordOnPriorities: Ordering[PriorityChain] = {
    implicit val ordOnVectorWithDoubles: Ordering[Vector[Double]] = {
      val cmp = new Comparator[Vector[Double]] {
        override def compare(a: Vector[Double], b: Vector[Double]): Int = {
          var i = 0
          while (i < a.size) {
            val left  = a(i)
            val right = b(i)
            if (left < right) return -1
            if (left > right) return 1
            i += 1
          }
          0
        }
      }
      Ordering.comparatorToOrdering(using cmp)
    }
    Ordering.by(_.data)
  }

  def apply(singleValue: Double): PriorityChain = PriorityChain(Vector(singleValue))

  def apply(multiValues: Double*): PriorityChain = PriorityChain(multiValues.toVector)
}
