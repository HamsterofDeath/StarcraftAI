import org.pmw.tinylog
import org.pmw.tinylog.{Configurator, Level}

/**
 * Created by HoD on 01.08.2015.
 */
package object pony {
  Configurator.defaultConfig().level(Level.TRACE).activate()

  type SCUnitType = Class[_ <: WrapsUnit]
  val tileSize = 32
  private var logLevel: LogLevel = Trace
  def setLogLevel_!(logLevel: LogLevel): Unit = { this.logLevel = logLevel }
  def error(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Error.includes(logLevel)) tinylog.Logger.error(a.toString)
  }
  def warn(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Warn.includes(logLevel)) tinylog.Logger.warn(a.toString)
  }
  def info(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Info.includes(logLevel)) tinylog.Logger.info(a.toString)
  }
  def debug(a: => Any, doIt: Boolean = true): Unit = {
    if (Debug.includes(logLevel)) tinylog.Logger.debug(a.toString)
  }
  def trace(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Trace.includes(logLevel)) tinylog.Logger.trace(a.toString)
  }
  sealed class LogLevel(val level: Int) {
    def includes(other: LogLevel) = level >= other.level
  }
  implicit class ToOneElemList[T](val t: T) extends AnyVal {
    def toSome = Some(t)
    def toSeq = Seq(t)
    def toList = List(t)
    def toJavaList = java.util.Arrays.asList(t)
    def toVector = Vector(t)
  }

  implicit class RichClass[T](val c: Class[_ <: T]) extends AnyVal {
    def className = {
      val lastDot = c.getName.lastIndexOf('.')
      val last$ = c.getName.lastIndexOf('$')
      c.getName.drop((lastDot max last$) + 1)
    }
  }

  implicit class RichUnitClass[T <: WrapsUnit](val c: Class[_ <: T]) extends AnyVal {
    def toUnitType = TypeMapping.unitTypeOf(c)
    def toMoreSpecific(race: SCRace) = TypeMapping.unitTypeOf(c)
  }

  case object Trace extends LogLevel(1)
  case object Debug extends LogLevel(2)
  case object Info extends LogLevel(3)
  case object Warn extends LogLevel(4)

  case object Error extends LogLevel(5)

  case object Off extends LogLevel(6)

}
