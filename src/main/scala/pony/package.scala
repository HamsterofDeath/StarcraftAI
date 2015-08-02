import org.pmw.tinylog

/**
 * Created by HoD on 01.08.2015.
 */
package object pony {
  type SCUnitType = Class[_ <: WrapsUnit]
  val tileSize = 32
  private var logLevel:LogLevel = Info
  def setLogLevel_!(logLevel: LogLevel):Unit = {this.logLevel = logLevel}
  def error(a : => Any):Unit = {
    if (Error.includes(logLevel)) tinylog.Logger.error(a.toString)
  }
  def warn(a : => Any):Unit = {
    if (Warn.includes(logLevel)) tinylog.Logger.warn(a.toString)
  }
  def info(a : => Any):Unit = {
    if (Info.includes(logLevel)) tinylog.Logger.info(a.toString)
  }
  def debug(a : => Any):Unit = {
    if (Debug.includes(logLevel)) tinylog.Logger.debug(a.toString)
  }
  def trace(a : => Any):Unit = {
    if (Trace.includes(logLevel)) tinylog.Logger.trace(a.toString)
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

  case object Trace extends LogLevel(1)
  case object Debug extends LogLevel(2)
  case object Info extends LogLevel(3)
  case object Warn extends LogLevel(4)
  case object Error extends LogLevel(5)

  case object Off extends LogLevel(6)
}
