package pony
package brain
package modules

case class RequestPlan(buildThese: Seq[EnqueueArmy#Ratio], needsSomething: Set[EnqueueArmy#Ratio])
