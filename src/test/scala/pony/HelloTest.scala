package pony

import org.specs2._

class HelloWorldTest extends Specification {

  def is = s2"""

 Sample test
              |Should say it's ok            $ok
              |1+1 should be 2               $math
                                 """.stripMargin

  def math = 1+1 shouldEqual 2
}