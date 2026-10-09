package pony
package brain
package modules
package strategy

import java.util.ServiceLoader
import scala.jdk.CollectionConverters._

/** Every known strategy plugin, sorted by key; two plugins may not share a key. */
final class StrategyRegistry(val plugins: Vector[StrategyPlugin]) {
  private val byKey = plugins.groupBy(_.key)

  byKey.find(_._2.size > 1).foreach { case (key, clash) =>
    throw new IllegalStateException(
      s"Strategy plugins ${clash.map(_.getClass.getName).mkString(", ")} share the key '$key'")
  }

  def keys: Vector[String] = plugins.map(_.key)

  def find(key: String): Option[StrategyPlugin] = byKey.get(key).map(_.head)
}

object StrategyRegistry {
  def discover(loader: ClassLoader = classOf[StrategyPlugin].getClassLoader): StrategyRegistry =
    new StrategyRegistry(ServiceLoader.load(classOf[StrategyPlugin], loader).asScala.toVector.sortBy(_.key))
}
