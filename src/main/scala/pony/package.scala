import org.pmw.tinylog
import org.pmw.tinylog.{Configurator, Level}

import scala.collection.mutable

/**
 * Created by HoD on 01.08.2015.
 */
package object pony {
  Configurator.defaultConfig().level(Level.TRACE).formatPattern("{level}:{message}").activate()

  def !!! : Nothing = !!!("Something is not as it should be")
  def !!!(msg: String): Nothing = throw new RuntimeException(msg)

  type SCUnitType = Class[_ <: WrapsUnit]
  val tileSize  = 32
  var tickCount = 0
  private var logLevel: LogLevel = Trace
  def setLogLevel_!(logLevel: LogLevel): Unit = { this.logLevel = logLevel }
  def error(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Error.includes(logLevel))
      tinylog.Logger.error(s"[$tick] ${a.toString}")
  }
  def tick = tickCount
  def warn(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Warn.includes(logLevel))
      tinylog.Logger.warn(s"[$tick] ${a.toString}")
  }
  def info(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Info.includes(logLevel))
      tinylog.Logger.info(s"[$tick] ${a.toString}")
  }
  def debug(a: => Any, doIt: Boolean = true): Unit = {
    if (Debug.includes(logLevel))
      tinylog.Logger.debug(s"[$tick] ${a.toString}")
  }
  def trace(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Trace.includes(logLevel))
      tinylog.Logger.trace(s"[$tick] ${a.toString}")
  }
  sealed class LogLevel(val level: Int) {
    def includes(other: LogLevel) = level >= other.level
  }
  implicit class RichOption[T](val o: Option[T]) extends AnyVal {
    def getOr(excuse: => String) = o match {
      case None => !!!(excuse)
      case Some(x) => x
    }
  }

  implicit class RichTraversableOnce[T](val t: TraversableOnce[T]) extends AnyVal {
    def minByOpt[C](cmp: T => C)(implicit cmp2: Ordering[C]) = {
      if (t.isEmpty) {
        None
      } else {
        Some(t.minBy(cmp))
      }
    }
    def maxByOpt[C](cmp: T => C)(implicit cmp2: Ordering[C]) = {
      if (t.isEmpty) {
        None
      } else {
        Some(t.maxBy(cmp))
      }
    }
  }

  implicit class ToOneElemList[T](val t: T) extends AnyVal {
    def toSome = Some(t)
    def toSeq = Seq(t)
    def toSet = Set(t)
    def toGSet = collection.Set(t)
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
  }

  implicit class InPlaceModify[T](val buff: mutable.Buffer[T]) extends AnyVal {
    def removeElem(elem: T): Unit = {
      val where = buff.indexOf(elem)
      assert(where >= 0, s"Not found: $elem in $buff")
      buff.remove(where)
    }

    def removeFirstMatch(elemIdentifier: T => Boolean): Unit = {
      val where = buff.indexWhere(elemIdentifier)
      assert(where >= 0, s"Not found $elemIdentifier in $buff")
      buff.remove(where)
    }
  }
  implicit class RichBoolean(val b: Boolean) extends AnyVal {
    def not = !b
    def ifElse[T](ifTrue: T, ifFalse: T) = if (b) ifTrue else ifFalse
  }
  case object Trace extends LogLevel(1)
  case object Debug extends LogLevel(2)
  case object Info extends LogLevel(3)

  case object Warn extends LogLevel(4)

  case object Error extends LogLevel(5)

  case object Off extends LogLevel(6)
}
