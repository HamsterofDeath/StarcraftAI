import bwapi.Game
import org.pmw.tinylog
import org.pmw.tinylog.{Configurator, Level}

import scala.collection.mutable

/**
 * Created by HoD on 01.08.2015.
 */
package object pony {
  setTinyLogLevel_!(Level.TRACE)

  def setTinyLogLevel_!(newLevel: Level) = {
    Configurator.defaultConfig().level(newLevel).formatPattern("{level}:{message}").activate()
  }

  def !!! : Nothing = !!!("Something is not as it should be")
  def !!!(msg: String): Nothing = throw new RuntimeException(msg)

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val exCon = global

  trait MMToImmutable[A, B] extends mutable.Map[A, mutable.Set[B]] {
    self =>
    def toImmutable = {
      self.map { case (k, v) => k -> v.toSet }.toMap
    }
  }

  type MultiMap[K, V] = mutable.HashMap[K, mutable.Set[V]]

  def multiMap[K, V] = new mutable.HashMap[K, mutable.Set[V]] with mutable.MultiMap[K, V] with MMToImmutable[K, V]
  type SCUnitType = Class[_ <: WrapsUnit]
  val tileSize  = 32
  var tickCount = 0
  private var setTinyLogLevel_! : LogLevel = LogLevels.Trace

  import LogLevels._

  def setLogLevel_!(logLevel: LogLevel): Unit = {
    setTinyLogLevel_!(logLevel.toTinyLogLevel)
    this.setTinyLogLevel_! = logLevel
  }
  def error(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Error.includes(setTinyLogLevel_!))
      tinylog.Logger.error(s"[$tick] ${a.toString}")
  }
  def tick = tickCount
  def warn(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Warn.includes(setTinyLogLevel_!))
      tinylog.Logger.warn(s"[$tick] ${a.toString}")
  }
  def info(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Info.includes(setTinyLogLevel_!))
      tinylog.Logger.info(s"[$tick] ${a.toString}")
  }
  def majorInfo(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Info.includes(setTinyLogLevel_!))
      tinylog.Logger.info(s"<MAJOR> [$tick] ${a.toString}")
  }
  def debug(a: => Any, doIt: Boolean = true): Unit = {
    if (Debug.includes(setTinyLogLevel_!))
      tinylog.Logger.debug(s"[$tick] ${a.toString}")
  }
  def trace(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && Trace.includes(setTinyLogLevel_!))
      tinylog.Logger.trace(s"[$tick] ${a.toString}")
  }
  abstract sealed class LogLevel(val level: Int) {
    def includes(other: LogLevel) = level >= other.level
    def toTinyLogLevel: Level
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

  implicit class RichIterator[T](val i: Iterator[T]) extends AnyVal {
    def nextOption() = {
      if (i.hasNext) Some(i.next()) else None
    }
  }

  implicit class ToOneElemList[T](val t: T) extends AnyVal {
    def toSome: Option[T] = Some(t)
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

    def retain(f: T => Boolean): Unit = {
      buff --= buff.filterNot(f)
    }

    def removeFirstMatch(elemIdentifier: T => Boolean): Unit = {
      val where = buff.indexWhere(elemIdentifier)
      assert(where >= 0, s"Not found $elemIdentifier in $buff")
      buff.remove(where)
    }

    def removeUntilInclusive(elemIdentifier: T => Boolean): Unit = {
      val where = buff.indexWhere(elemIdentifier)
      assert(where >= 0, s"Not found $elemIdentifier in $buff")
      buff.remove(0, where + 1)
    }
  }
  implicit class RichBoolean(val b: Boolean) extends AnyVal {
    def not = !b
    def ifElse[T](ifTrue: T, ifFalse: T) = if (b) ifTrue else ifFalse
  }
  implicit class RichBitSet(val b: mutable.BitSet) extends AnyVal {
    def immutableCopy = collection.immutable.BitSet.fromBitMaskNoCopy(b.toBitMask)
    def immutableWrapper = b.toImmutable
  }
  object LogLevels {
    case object Trace extends LogLevel(1) {
      override def toTinyLogLevel = Level.TRACE
    }
    case object Debug extends LogLevel(2) {
      override def toTinyLogLevel = Level.DEBUG
    }
    case object Info extends LogLevel(3) {
      override def toTinyLogLevel = Level.INFO
    }

    case object Warn extends LogLevel(4) {
      override def toTinyLogLevel = Level.WARNING
    }

    case object Error extends LogLevel(5) {
      override def toTinyLogLevel = Level.ERROR
    }

    case object Off extends LogLevel(6) {
      override def toTinyLogLevel = Level.OFF
    }
  }


  implicit class GameWrap(val game: Game) extends AnyVal {
    def suggestFileName = s"${game.mapName()}_${game.mapHash()}.bin"
  }

}
