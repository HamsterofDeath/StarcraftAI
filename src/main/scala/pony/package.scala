import bwapi.Game
import org.pmw.tinylog
import org.pmw.tinylog.writers.FileWriter
import org.pmw.tinylog.{Configurator, Level}
import pony.LogLevels.{LogError, LogWarn}

import scala.annotation.elidable
import scala.collection.mutable
import scala.concurrent.ExecutionContext

/**
  * Created by HoD on 01.08.2015.
  */
package object pony {

  // milestone 3:
  // TODO collect minerals/gas from far away if out of resources in bases
  // TODO fix upgrade building counts (too many engineering bays, not enough of the other one)
  // TODO stay away from stationary things if long range units are available
  // TODO send nearby units to help in battles
  // TODO build units best suited against enemy

  // milestone 4:
  // TODO send only necessary units for defenses, use "battle simulator" to estimate which units
  // are required for each attack/defense
  // TODO optimize worker paths -> might be impossible
  // TODO cover island maps

  // milestone 4:
  //  ...

  // milestone 5:
  // TODO create technologial singularity

  type MultiMap[K, V] = mutable.HashMap[K, mutable.Set[V]]
  type SCUnitType = Class[_ <: WrapsUnit]
  val memoryHog = false

  setTinyLogLevel_!(Level.TRACE)
  implicit val exCon = ExecutionContext.global
  val tileSize  = 32
  var tickCount = 0

  private var tinyLogLevel: LogLevel = LogLevels.LogTrace

  def !!! : Nothing = !!!("Something is not as it should be")

  def !!!(msg: String): Nothing = throw new RuntimeException(msg)

  def multiMap[K, V] = new mutable.HashMap[K, mutable.Set[V]] with mutable.MultiMap[K, V] with
                           MMToImmutable[K, V]

  def setLogLevel_!(logLevel: LogLevel): Unit = {
    setTinyLogLevel_!(logLevel.toTinyLogLevel)
    this.tinyLogLevel = logLevel
  }

  def setTinyLogLevel_!(newLevel: Level) = {
    val ok = Configurator.defaultConfig()
             .removeAllWriters()
             .level(newLevel)
             .formatPattern("{level}:{message}")
             .addWriter(new FileWriter("log/match.log", false), newLevel, "{level}:{message}")
             .activate()
    assert(ok)
  }

  def logLevel = tinyLogLevel

  @elidable(LOGLEVEL)
  def error(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && LogError.includes(tinyLogLevel))
      tinylog.Logger.error(s"[$tick] ${a.toString}")
  }

  @elidable(LOGLEVEL)
  def warn(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && LogWarn.includes(tinyLogLevel))
      tinylog.Logger.warn(s"[$tick] ${a.toString}")
  }

  setLogLevel_!(LogLevels.LogTrace)

  import LogLevels._

  @elidable(LOGLEVEL)
  def info(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && LogInfo.includes(tinyLogLevel))
      tinylog.Logger.info(s"[$tick] ${a.toString}")
  }

  def tick = tickCount

  @elidable(LOGLEVEL)
  def majorInfo(a: => Any, doIt: Boolean = true): Unit = {
    if (doIt && LogInfo.includes(tinyLogLevel))
      tinylog.Logger.info(s"<MAJOR> [$tick] ${a.toString}")
  }

  @elidable(LOGLEVEL)
  def debug(a: => Any, doIt: Boolean = true): Unit = {
    if (LogDebug.includes(tinyLogLevel))
      tinylog.Logger.debug(s"[$tick] ${a.toString}")
  }

  private val LOGLEVEL = 500

  @elidable(LOGLEVEL)
  def trace(a: => Any, doIt: Boolean = true, marker: String = ""): Unit = {
    if (doIt && LogTrace.includes(tinyLogLevel))
      tinylog.Logger.trace(s"[$tick] ${if (marker.isEmpty) "" else s"[$marker] "}${a.toString}")
  }

  trait MMToImmutable[A, B] extends mutable.Map[A, mutable.Set[B]] {
    self =>
    def toImmutable = {
      self.map { case (k, v) => k -> v.toSet }.toMap
    }
  }

  abstract sealed class LogLevel(val level: Int) {
    def includes(other: LogLevel) = level >= other.level

    def toTinyLogLevel: Level
  }

  class PrimeNumber(val i: Int) extends AnyVal

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

  implicit class RichTraversable[T](val t: Traversable[T]) extends AnyVal {
    def headAssert = {
      assert(t.size == 1)
      t.head
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

    def maxOpt(implicit cmp2: Ordering[T]) = {
      if (t.isEmpty) None else t.max.toSome
    }

    def minOpt(implicit cmp2: Ordering[T]) = {
      if (t.isEmpty) None else t.min.toSome
    }

    def maxByOpt[C](cmp: T => C)(implicit cmp2: Ordering[C]) = {
      if (t.isEmpty) {
        None
      } else {
        Some(t.maxBy(cmp))
      }
    }

    def minByOptFiltered[C](cmp: T => C)(check: C => Boolean)(implicit cmp2: Ordering[C]) = {
      if (t.isEmpty) {
        None
      } else {
        Some(t.minBy(cmp)).filter(e => check(cmp(e)))
      }
    }

    def maxByOptFiltered[C](cmp: T => C)(check: C => Boolean)(implicit cmp2: Ordering[C]) = {
      if (t.isEmpty) {
        None
      } else {
        Some(t.maxBy(cmp)).filter(e => check(cmp(e)))
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

  implicit def unwrap[T](f: FutureIterator[_, T]): Option[T] = f.mostRecent

  implicit class RichAny[T](val any: T) extends AnyVal {
    def nullSafe[R](f: T => R) = if (any != null) Some(f(any)) else None
  }

  implicit class RichBitSet(val b: mutable.BitSet) extends AnyVal {
    def immutableCopy = collection.immutable.BitSet.fromBitMaskNoCopy(b.toBitMask)

    def immutableWrapper = b.toImmutable
  }

  implicit class GameWrap(val game: Game) extends AnyVal {
    def suggestFileName = s"${game.mapName()}_${game.mapHash()}.bin"
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
      override def toTinyLogLevel = Level.WARNING
    }

    case object LogError extends LogLevel(5) {
      override def toTinyLogLevel = Level.ERROR
    }

    case object LogOff extends LogLevel(6) {
      override def toTinyLogLevel = Level.OFF
    }

  }

}
