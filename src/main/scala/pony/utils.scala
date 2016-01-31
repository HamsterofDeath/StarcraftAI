package pony

import java.io.{BufferedInputStream, ByteArrayInputStream, ByteArrayOutputStream, File,
ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.zip.{Deflater, ZipEntry, ZipInputStream, ZipOutputStream}

import bwapi.Game
import org.apache.commons.io.FileUtils

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class Renderer(game: Game, private var color: bwapi.Color) {
  def drawLine(from: MapTilePosition, to: MapTilePosition): Unit = {
    game.drawLineMap(from.mapX, from.mapY, to.mapX, to.mapY, color)
  }

  def drawLineInTile(from: MapTilePosition, to: MapTilePosition): Unit = {
    game.drawLineMap(from.mapX + 16, from.mapY + 16, to.mapX + 16, to.mapY + 16, color)
  }

  def drawStar(where: MapTilePosition, size: Int = 3): Unit = {
    game.drawLineMap(where.movedBy(-size, -size).nativeMapPosition,
      where.movedBy(size, size).nativeMapPosition, color)
    game.drawLineMap(where.movedBy(size, -size).nativeMapPosition,
      where.movedBy(-size, size).nativeMapPosition, color)
    game.drawLineMap(where.movedBy(-size, 0).nativeMapPosition,
      where.movedBy(size, 0).nativeMapPosition, color)
    game.drawLineMap(where.movedBy(0, -size).nativeMapPosition,
      where.movedBy(0, size).nativeMapPosition, color)
  }

  def drawLine(from: MapPosition, to: MapPosition): Unit = {
    game.drawLineMap(from.x, from.y, to.x, to.y, color)
  }

  def drawTextOnScreen(text: String, row: Int = 0): Unit = {
    game.drawTextScreen(10, 10 + row * 10, text)
  }

  def drawTextAtTile(text: String, where: MapTilePosition): Unit = {
    game.drawTextMap(where.mapX, where.mapY, text)
  }

  def drawOutline(where: Area): Unit = {
    drawOutline(where.upperLeft.mapX, where.upperLeft.mapY, where.lowerRight.mapX + tileSize,
      where.lowerRight.mapY + tileSize)
  }

  def drawOutline(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    game.drawBoxMap(x1, y1, x2, y2, color)
  }

  def drawTextAtMobileUnit(u: Mobile, text: String, lineOffset: Int = 0): Unit = {
    game
    .drawTextMap(u.currentPositionNative.getX, u.currentPositionNative.getY + lineOffset * 10, text)
  }

  def drawTextAtStaticUnit(u: StaticallyPositioned, text: String, lineOffset: Int = 0): Unit = {
    game.drawTextMap(u.nativeMapPosition.getX, u.nativeMapPosition.getY + lineOffset * 10, text)
  }

  def indicateTarget(currentPosition: MapTilePosition, to: MapTilePosition): Unit = {
    indicateTarget(currentPosition.asMapPosition, to)
  }

  def indicateTarget(currentPosition: MapPosition, to: MapTilePosition): Unit = {
    game.drawLineMap(currentPosition.x + tileSize / 2,
      currentPosition.y + tileSize / 2,
      to.mapX + tileSize / 2,
      to.mapY + tileSize / 2,
      color)
    drawCircleAroundTile(to)
  }

  def drawCircleAroundTile(around: MapTilePosition): Unit = {
    game.drawCircleMap(around.mapX + tileSize / 2, around.mapY + tileSize / 2, tileSize / 2, color)
  }

  def indicateTarget(currentPosition: MapPosition, to: MapPosition): Unit = {
    game.drawLineMap(currentPosition.x, currentPosition.y, to.x, to.y, color)
    drawCircleAround(to)
  }

  def drawCircleAround(around: MapPosition): Unit = {
    drawCircleAround(around, tileSize / 2)
  }

  def drawCircleAround(around: MapTilePosition): Unit = {
    drawCircleAround(around.asMapPosition, tileSize / 2)
  }

  def drawCircleAround(around: MapPosition, radiusPixels: Int): Unit = {
    game.drawCircleMap(around.x, around.y, radiusPixels, color)
  }

  def indicateTarget(currentPosition: MapPosition, area: Area): Unit = {
    game.drawLineMap(currentPosition.x, currentPosition.y, area.center.x, area.center.y, color)
    markTarget(area)
  }

  def markTarget(area: Area): Unit = {
    val center = area.center
    val radius = (area.sizeOfArea.x + area.sizeOfArea.y) / 2
    game.drawCircleMap(center.x, center.y, radius, color)
  }

  def writeText(position: MapTilePosition, msg: Any): Unit = {
    game.drawTextMap(position.x * 32, position.y * 32, msg.toString)
  }

  def in_!(color: bwapi.Color) = {
    this.color = color
    this
  }

  def drawCrossedOutOnTile(x: Int, y: Int): Unit = {
    drawCrossedOutOnTile(MapTilePosition.shared(x, y))
  }

  def drawCrossedOutOnTile(p: MapTilePosition): Unit = {
    game.drawBoxMap(p.x * 32, p.y * 32, p.x * 32 + tileSize, p.y * 32 + tileSize, color)
    game.drawLineMap(p.x * 32, p.y * 32, p.x * 32 + tileSize, p.y * 32 + tileSize, color)
    game.drawLineMap(p.x * 32 + tileSize, p.y * 32, p.x * 32, p.y * 32 + tileSize, color)
  }
}

class LazyVal[T](gen: => T, onValueChange: Option[() => Unit] = None) extends Serializable {
  private var allowUnsafe    = false
  private val creationThread = Thread.currentThread()
  private var locked         = false
  private var evaluated      = false
  private var value: T       = _

  def allowMultithreading_!() = {
    allowUnsafe = true
    this
  }

  def lockValueForever(): Unit = {
    locked = true
  }

  def invalidate(): Unit = {
    if (!locked) {
      evaluated = false
      if (onValueChange.isEmpty) {
        value = null.asInstanceOf[T]
      }
    }
  }

  override def toString = s"LazyVal($get)"

  def get = {
    assert(allowUnsafe || Thread.currentThread() == creationThread)
    if (!evaluated)
      if (onValueChange.isDefined) {
        val newVal = gen
        if (newVal != value) {
          onValueChange.foreach(_ ())
          value = newVal
        }
      } else {
        value = gen
        assert(value != null)
      }

    evaluated = true
    assert(value != null)
    value
  }
}

object LazyVal {
  def from[T](t: => T) = new LazyVal(t, None)

  def from[T](t: => T, onValueChange: => Unit) = new LazyVal(t, Some(() => onValueChange))
}

class FileStorageLazyVal[T](gen: => T, fileName: String) extends LazyVal(gen, None) {

  private var loaded = false

  override def invalidate(): Unit = {
    loaded = false
    file.delete()
    super.invalidate()
  }

  override def get: T = {
    if (loaded) {
      super.get
    } else {
      if (file.exists()) {
        info(s"Loading ${file.getAbsolutePath}")
        val bytes = FileUtils.readFileToByteArray(file)
        loaded = true
        fromZippedBytes(bytes) match {
          case None =>
            invalidate()
            get
          case Some(data) => data
        }
      } else {
        val saveMe = super.get
        info(s"Saving ${file.getAbsolutePath}")
        FileUtils.writeByteArrayToFile(file, toZippedBytes(saveMe))
        loaded = true
        saveMe
      }
    }
  }

  def fromZippedBytes(bytes: Array[Byte]) = {
    val zi = new ZipInputStream(new ByteArrayInputStream(bytes))
    val nextEntry = zi.getNextEntry
    val os = new ObjectInputStream(new BufferedInputStream(zi))
    Try {
      val ret = os.readObject().asInstanceOf[T]
      os.close()
      ret
    }.toOption
  }

  private def file = FileStorageLazyVal.fileByName(fileName)

  private def toZippedBytes(t: T): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val o = new ObjectOutputStream(bytes)
    o.writeObject(t)
    val data = bytes.toByteArray
    val zipped = new ByteArrayOutputStream()
    val zo = new ZipOutputStream(zipped)
    zo.setLevel(Deflater.BEST_COMPRESSION)
    zo.putNextEntry(new ZipEntry("pony.magic"))
    zo.write(data)
    zo.closeEntry()
    zo.close()
    zipped.toByteArray
  }
}

object FileStorageLazyVal {
  def fromFunction[T](gen: => T, unique: String) = new FileStorageLazyVal(gen, unique)

  def fileByName(fileName: String) = {
    initRoot()
    new File(s"data/$fileName.dat")
  }

  def initRoot(): Unit = {
    val file = new File("data")
    if (!file.exists()) {
      info(s"Data directory is ${file.getAbsolutePath}")
      assert(file.mkdir())
    }
  }
}

class BWFuture[+T](val future: Future[T], incomplete: T) {

  def blockAndGet = {
    Await.result(future, Duration.Inf)
  }

  def assumeDoneAndGet = {
    assert(future.isCompleted)
    result
  }

  def idle = isDone

  def isDone = future.isCompleted

  def map[X](f: T => X) = new BWFuture(future.map(f), f(incomplete))

  def ifDone[X](ifDone: T => X): Unit = {
    if (future.isCompleted) {
      ifDone(result)
    }
  }

  def result = future.value match {
    case Some(Success(x)) => x
    case Some(Failure(e)) => throw e
    case _ => incomplete
  }

  def matchOnSelf[X](ifDone: T => X, ifRunning: => X) = {
    if (future.isCompleted) {
      ifDone(result)
    } else {
      ifRunning
    }
  }

}

class FutureIterator[IN, T](feed: => IN, produce: IN => T, startNow: Boolean) {
  private val lock       = new ReentrantReadWriteLock()
  private var lastFeed   = Option.empty[IN]
  private var done       = Option.empty[T]
  private var inProgress = if (startNow) nextFuture else BWFuture.none
  private var thinking   = startNow

  def mapOnContent[X](f: T => X) = {
    mostRecent.map(f)
  }

  def flatMapOnContent[X](f: T => Option[X]) = {
    mostRecent.flatMap(f)
  }

  def hasResult = mostRecent.isDefined

  def mostRecent = {
    lock.readLock().lock()
    val x = done
    lock.readLock().unlock()
    x
  }

  def feedObj = feed

  def lastUsedFeed = {
    lock.readLock().lock()
    val x = lastFeed
    lock.readLock().unlock()
    x
  }

  def mostRecentAssumeCalculated = mostRecent.get

  def prepareNextIfDone(): Unit = {
    lock.writeLock().lock()
    if (!thinking) {
      thinking = true
      lastFeed = None
      inProgress = nextFuture
    }
    lock.writeLock().unlock()
  }

  private def nextFuture = {
    val input = feed
    val fut = BWFuture.produceFrom(produce(input))
    fut.future.onSuccess {
      case any =>
        lock.writeLock().lock()
        thinking = false
        done = any
        lastFeed = Some(input)
        lock.writeLock().unlock()
    }
    fut
  }
}

object FutureIterator {
  def feed[IN](in: => IN) = new {
    def produceAsync[T](produce: IN => T) = {
      new FutureIterator(in, produce, true)
    }

    def produceAsyncLater[T](produce: IN => T) = {
      new FutureIterator(in, produce, false)
    }
  }
}

object BWFuture {

  def none[T] = apply(Option.empty[T])

  def apply[T](produce: => Option[T]): BWFuture[Option[T]] = BWFuture(produce, None)

  def apply[T](produce: => T, ifIncomplete: T) = {
    val fut = Future {produce}
    new BWFuture(fut, ifIncomplete)
  }

  def from[T](produce: => T): BWFuture[Option[T]] = {
    BWFuture(Some(produce), None)
  }

  def produceFrom[T](produce: => T) = BWFuture(Some(produce))

  implicit class Result[T](val fut: BWFuture[Option[T]]) extends AnyVal {
    def orElse(other: T) = if (fut.isDone) fut.result.get else other

    def imap[X](f: T => X) = fut.map(_.map(f))

    def ifDoneOpt[X](ifDone: T => X): Unit = {
      fut.ifDone(op => ifDone(op.get))
    }

    def foldOpt[X](ifRunning: => X)(ifDone: T => X) = {
      matchOnOptSelf(ifDone, ifRunning)
    }

    def matchOnOptSelf[X](ifDone: T => X, ifRunning: => X) = {
      fut.matchOnSelf({
        case Some(t) => ifDone(t)
        case _ => ifRunning
      }, ifRunning)
    }
  }

}
