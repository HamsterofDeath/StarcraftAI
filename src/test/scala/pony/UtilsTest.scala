package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.PriorityChain

class UtilsTest extends Specification with MustMatchers {

  import PriorityChain._

  def priorityChain = {
    {
      val min = PriorityChain(0, 0, 0)
      val max = PriorityChain(0, 0, 1)
      List(min, max).max === max
    }
    {
      val min = PriorityChain(0, 0, 1)
      val max = PriorityChain(0, 0, 2)
      List(min, max).max === max
    }
    {
      val min = PriorityChain(0, 0, 1)
      val max = PriorityChain(0, 1, 2)
      List(min, max).max === max
    }
    {
      val min = PriorityChain(0, 0, 1)
      val max = PriorityChain(0, 1, 0)
      List(min, max).max === max
    }
    {
      val min = PriorityChain(0, 9, 9)
      val max = PriorityChain(1, 0, 0)
      List(min, max).max === max
    }
  }

  def is =
    s2"""
       |Priority chain should just work $priorityChain
                                 """.stripMargin

}