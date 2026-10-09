import java.math.RoundingMode
import java.text.DecimalFormat

import bwapi.Game
import org.tinylog.Level
import org.tinylog.Logger
import org.tinylog.configuration.Configuration
import pony.LogLevels.{LogError, LogWarn}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

/**
  * Created by HoD on 01.08.2015.
  */
package object pony {

  // milestone 3:
  // TODO collect minerals/gas from far away if out of resources in bases
  // TODO optimize performance

  // milestone 3.5
  // TODO build units best suited against enemy
  // TODO handle remaining special abilities, add rules to handle abilities of enemy

  // milestone 4:
  // TODO send only necessary units for defenses, use "battle simulator" to estimate which units
  // are required for each attack/defense
  // TODO cover island maps

  // milestone 4:
  //  ...

  // milestone 5:
  // TODO create technologial singularity

  type SCUnitType = Class[? <: WrapsUnit]
  val memoryHog = false

  setTinyLogLevel_!(Level.TRACE)
  implicit val exCon: ExecutionContext = ExecutionContext.global
  val tileSize  = 32

  private var tinyLogLevel: LogLevel = LogLevels.LogTrace

  def !!! : Nothing = !!!("Something is not as it should be")

  def !!!(msg: String): Nothing = throw new RuntimeException(msg)

  def multiMap[K, V] = new MultiMap[K, V]

  def setLogLevel_!(logLevel: LogLevel): Unit = {
    setTinyLogLevel_!(logLevel.toTinyLogLevel)
    this.tinyLogLevel = logLevel
  }

  def setTinyLogLevel_!(newLevel: Level): Unit = {
    new java.io.File("log").mkdirs()
    val levelName = newLevel.toString.toLowerCase
    val properties = new java.util.HashMap[String, String]()
    properties.put("level", levelName)
    properties.put("writer", "file")
    properties.put("writer.file", "log/match.log")
    properties.put("writer.append", "false")
    properties.put("writer.format", "{level}:{message}")
    properties.put("writer.level", levelName)
    Configuration.replace(properties)
  }

  def logLevel = tinyLogLevel

  def error(a: => Any, doIt: Boolean = true): Unit = {
    if (LogError.includes(tinyLogLevel) && doIt)
      Logger.error("{}", s"[$tick] ${a.toString}")
  }

  def warn(a: => Any, doIt: Boolean = true): Unit = {
    if (LogWarn.includes(tinyLogLevel) && doIt)
      Logger.warn("{}", s"[$tick] ${a.toString}")
  }

  setLogLevel_!(LogLevels.LogInfo)

  import LogLevels._

  def info(a: => Any, doIt: Boolean = true): Unit = {
    if (LogInfo.includes(tinyLogLevel) && doIt)
      Logger.info("{}", s"[$tick] ${a.toString}")
  }

  def tick = TickCounter.tickCount

  def majorInfo(a: => Any, doIt: Boolean = true): Unit = {
    if (LogInfo.includes(tinyLogLevel) && doIt)
      Logger.info("{}", s"<MAJOR> [$tick] ${a.toString}")
  }

  def debug(a: => Any, doIt: Boolean = true): Unit = {
    if (LogDebug.includes(tinyLogLevel) && doIt)
      Logger.debug("{}", s"[$tick] ${a.toString}")
  }

  def trace(a: => Any, doIt: Boolean = true, marker: String = ""): Unit = {
    if (LogTrace.includes(tinyLogLevel) && doIt)
      Logger.trace("{}", s"[$tick] ${if (marker.isEmpty) "" else s"[$marker] "}${a.toString}")
  }

  abstract sealed class LogLevel(val level: Int) {
    def includes(other: LogLevel) = level >= other.level

    def toTinyLogLevel: Level
  }

  class PrimeNumber(val i: Int) extends AnyVal

  implicit class RichLong(val l: Long) extends AnyVal {
    def nanoToMillis = l.toDouble / 1000 / 1000
  }

  implicit class RichOption[T](val o: Option[T]) extends AnyVal {
    def getOr(excuse: => String) = o match {
      case None => !!!(excuse)
      case Some(x) => x
    }

    def forNone[T2](x: => T2) = {
      if (o.isEmpty) {
        x
      }
    }
  }

  implicit class RichInt(val i: Int) extends AnyVal {
    def toBase36 = Integer.toString(i, 36)
  }

  implicit class RichDouble(val d: Double) extends AnyVal {

    def format: String = format(2)

    def format(decimal: Int): String = {
      val df = new DecimalFormat()
      df.setRoundingMode(RoundingMode.HALF_UP)
      df.setMaximumFractionDigits(2)
      df.format(d)
    }
  }

  implicit class RichMutableTraversable[T](val t: mutable.Iterable[T]) extends AnyVal {
    // not correct, but i only use it in ways so that it doesn't matter
    def immutableView = t.toSeq
  }

  implicit class RichIterable[T](val t: Iterable[T]) extends AnyVal {
    def headAssert = {
      assert(t.size == 1)
      t.head
    }
  }

  implicit class RichIterableOnce[T](val t: IterableOnce[T]) extends AnyVal {

    def minByOpt[C](cmp: T => C)(implicit cmp2: Ordering[C]) = {
      if (t.iterator.isEmpty) {
        None
      } else {
        Some(t.iterator.minBy(cmp))
      }
    }

    def maxOpt(implicit cmp2: Ordering[T]) = {
      if (t.iterator.isEmpty) None else t.iterator.max.toSome
    }

    def minOpt(implicit cmp2: Ordering[T]) = {
      if (t.iterator.isEmpty) None else t.iterator.min.toSome
    }

    def maxByOpt[C](cmp: T => C)(implicit cmp2: Ordering[C]) = {
      if (t.iterator.isEmpty) {
        None
      } else {
        Some(t.iterator.maxBy(cmp))
      }
    }

    def minByOptFiltered[C](cmp: T => C)(check: C => Boolean)(implicit cmp2: Ordering[C]) = {
      if (t.iterator.isEmpty) {
        None
      } else {
        Some(t.iterator.minBy(cmp)).filter(e => check(cmp(e)))
      }
    }

    def maxByOptFiltered[C](cmp: T => C)(check: C => Boolean)(implicit cmp2: Ordering[C]) = {
      if (t.iterator.isEmpty) {
        None
      } else {
        Some(t.iterator.maxBy(cmp)).filter(e => check(cmp(e)))
      }
    }
  }

  implicit class RichIterator[T](val i: Iterator[T]) extends AnyVal {
    def nextOption() = {
      if (i.hasNext) Some(i.next()) else None
    }
  }

  implicit class RichMap[K, V](val m: Map[K, V]) extends AnyVal {
    def mapValuesStrict[V2](f: V => V2) = {
      m.map { case (k, v) => k -> f(v) }
    }
  }

  implicit class RichMutableMap[K, V](val m: mutable.Map[K, V]) extends AnyVal {
    def insertReplace(k: K, f: V => V, initial: V) = {
      m.get(k) match {
        case None => m.put(k, initial)
        case Some(old) => m.put(k, f(old))
      }
    }
  }

  class ToOneElemList[T](val t: T) extends AnyVal {
    def toSome: Option[T] = Some(t)

    def toSeq = Seq(t)

    def toSet = Set(t)

    def toGSet = collection.Set(t)

    def toList = List(t)

    def toJavaList = java.util.Arrays.asList(t)

    def toVector = Vector(t)
  }

  implicit def toOneElemList[T](t: T)(using
      scala.util.NotGiven[T <:< Array[?]],
      scala.util.NotGiven[T <:< String],
      scala.util.NotGiven[T <:< IterableOnce[?]]
  ): ToOneElemList[T] = new ToOneElemList(t)

  implicit class RichClass[T](val c: Class[? <: T]) extends AnyVal {
    def >=(other: Class[?]) = c.isAssignableFrom(other)

    def <=(other: Class[?]) = other.isAssignableFrom(c)

    def className = {
      val lastDot = c.getName.lastIndexOf('.')
      val lastDollar = c.getName.lastIndexOf('$')
      c.getName.drop((lastDot max lastDollar) + 1)
    }
  }

  implicit class RichUnitClass[T <: WrapsUnit](val c: Class[? <: T]) extends AnyVal {
    def toUnitType = TypeMapping.unitTypeOf(c)
  }

  implicit class InPlaceModify[T](val buff: mutable.Buffer[T]) extends AnyVal {
    def removeElem(elem: T): Unit = {
      val where = buff.indexOf(elem)
      assert(where >= 0, s"Not found: $elem in $buff")
      buff.remove(where)
    }

    def retain(f: T => Boolean) = {
      buff --= buff.filterNot(f)
      buff
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

    def someIfTrue[T](ifTrue: => T) = if (b) Some(ifTrue) else None
  }

  implicit def unwrap[T](lv: LazyVal[T]): T = lv.get

  implicit def unwrap[T](f: FutureIterator[?, T]): Option[T] = f.mostRecent

  implicit class RichAny[T](val any: T) extends AnyVal {
    def nullSafe[R](f: T => R) = if (any != null) Some(f(any)) else None
  }

  implicit class RichAnyRef[T](val anyRef: T) extends AnyVal {
    def wrapNull = Option(anyRef)
  }

  implicit class RichBitSet(val b: mutable.BitSet) extends AnyVal {
    def immutableCopy = collection.immutable.BitSet.fromBitMaskNoCopy(b.toBitMask)

    def immutableWrapper = b.toImmutable
  }

  implicit class GameWrap(val game: Game) extends AnyVal {
    def suggestFileName = s"${game.mapHash()}.bin"
  }

  object Primes {
    val prime2   = new PrimeNumber(2)
    val prime3   = new PrimeNumber(3)
    val prime5   = new PrimeNumber(5)
    val prime7   = new PrimeNumber(7)
    val prime11  = new PrimeNumber(11)
    val prime13  = new PrimeNumber(13)
    val prime17  = new PrimeNumber(17)
    val prime19  = new PrimeNumber(19)
    val prime23  = new PrimeNumber(23)
    val prime29  = new PrimeNumber(29)
    val prime31  = new PrimeNumber(31)
    val prime37  = new PrimeNumber(37)
    val prime41  = new PrimeNumber(41)
    val prime43  = new PrimeNumber(43)
    val prime47  = new PrimeNumber(47)
    val prime53  = new PrimeNumber(53)
    val prime59  = new PrimeNumber(59)
    val prime61  = new PrimeNumber(61)
    val prime67  = new PrimeNumber(67)
    val prime71  = new PrimeNumber(71)
    val prime73  = new PrimeNumber(73)
    val prime79  = new PrimeNumber(79)
    val prime83  = new PrimeNumber(83)
    val prime89  = new PrimeNumber(89)
    val prime97  = new PrimeNumber(97)
    val prime101 = new PrimeNumber(101)
    val prime103 = new PrimeNumber(103)
    val prime107 = new PrimeNumber(107)
    val prime109 = new PrimeNumber(109)
    val prime113 = new PrimeNumber(113)
    val prime127 = new PrimeNumber(127)
    val prime131 = new PrimeNumber(131)
    val prime137 = new PrimeNumber(137)
    val prime139 = new PrimeNumber(139)
    val prime149 = new PrimeNumber(149)
    val prime151 = new PrimeNumber(151)
    val prime157 = new PrimeNumber(157)
    val prime163 = new PrimeNumber(163)
    val prime167 = new PrimeNumber(167)
    val prime173 = new PrimeNumber(173)
    val prime179 = new PrimeNumber(179)
    val prime181 = new PrimeNumber(181)
    val prime191 = new PrimeNumber(191)
    val prime193 = new PrimeNumber(193)
    val prime197 = new PrimeNumber(197)
    val prime199 = new PrimeNumber(199)
    val prime211 = new PrimeNumber(211)
    val prime223 = new PrimeNumber(223)
    val prime227 = new PrimeNumber(227)
    val prime229 = new PrimeNumber(229)
    val prime233 = new PrimeNumber(233)
    val prime239 = new PrimeNumber(239)
    val prime241 = new PrimeNumber(241)
    val prime251 = new PrimeNumber(251)
  }

  object LogLevels {

    case object LogTrace extends LogLevel(1) {
      override def toTinyLogLevel = Level.TRACE
    }

    case object LogDebug extends LogLevel(2) {
      override def toTinyLogLevel = Level.DEBUG
    }

    case object LogInfo extends LogLevel(3) {
      override def toTinyLogLevel = Level.INFO
    }

    case object LogWarn extends LogLevel(4) {
      override def toTinyLogLevel = Level.WARN
    }

    case object LogError extends LogLevel(5) {
      override def toTinyLogLevel = Level.ERROR
    }

    case object LogOff extends LogLevel(6) {
      override def toTinyLogLevel = Level.OFF
    }

  }

}
