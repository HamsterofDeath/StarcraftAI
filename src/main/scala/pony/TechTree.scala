package pony

import scala.collection.mutable

trait TechTree {
  lazy val requiredBy = {
    val result = multiMap[Class[? <: WrapsUnit], Class[? <: WrapsUnit]]
    dependsOn.foreach { case (target, needs) =>
      needs.foreach { c =>
        result.addBinding(c, target)
      }
    }
    result.toImmutable
  }
  protected val builtBy: Map[Class[? <: WrapsUnit], Class[? <: WrapsUnit]]
  protected val dependsOn: Map[Class[? <: WrapsUnit], Set[? <: Class[? <: Building]]]
  protected val upgrades: Map[Upgrade, Class[? <: Upgrader]]
  private val requirementsCache = mutable.HashMap
    .empty[Class[? <: WrapsUnit], Set[Class[? <: Building]]]
  def mainBuildingOf(addon: Class[? <: Addon]) = {
    builtBy(addon).asInstanceOf[Class[? <: CanBuildAddons]]
  }
  def upgraderFor(upgrade: Upgrade) = upgrades(upgrade)

  def canBuild(factory: Class[? <: UnitFactory], mobile: Class[? <: Mobile]) = {
    builtBy(mobile) == factory
  }
  def canUpgrade(u: Class[? <: Upgrader], up: Upgrade)                          = upgrades(up) == u
  def canBuildAddon(main: Class[? <: CanBuildAddons], addon: Class[? <: Addon]) = {
    builtBy(addon) == main
  }
  def requiredFor[T <: WrapsUnit](what: Class[? <: T]) = {
    requirementsCache.getOrElseUpdate(
      what, {
        val all  = mutable.Set.empty[Class[? <: Building]]
        var head = mutable.Set.empty[Class[? <: Building]] ++= dependsOn.getOrElse(what, Set.empty)
        while (head.nonEmpty) {
          all ++= head
          head = head.flatMap(e => dependsOn.getOrElse(e, Set.empty))
        }
        all.toSet
      }
    )
  }
}
