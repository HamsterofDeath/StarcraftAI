package pony
package brain
package modules
package production

case class RequestPlan(buildThese: Seq[EnqueueArmy#Ratio], needsSomething: Set[EnqueueArmy#Ratio])
