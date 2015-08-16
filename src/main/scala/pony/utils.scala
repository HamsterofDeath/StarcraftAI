package pony

import java.io.{BufferedInputStream, ByteArrayInputStream, ByteArrayOutputStream, File, ObjectInputStream,
ObjectOutputStream}
import java.util.zip.{Deflater, ZipEntry, ZipInputStream, ZipOutputStream}

import bwapi.Game
import org.apache.commons.io.FileUtils

import scala.util.Try

class Renderer(game: Game, private var color: bwapi.Color) {
  def drawLine(from: MapTilePosition, to: MapTilePosition): Unit = {
    game.drawLineMap(from.mapX, from.mapY, to.mapX, to.mapY, color)
  }

  def drawTextOnScreen(text: String, row: Int = 0): Unit = {
    game.drawTextScreen(10, 10 + row * 10, text)
  }

  def drawOutline(where: Area): Unit = {
    drawOutline(where.upperLeft.mapX, where.upperLeft.mapY, where.lowerRight.mapX, where.lowerRight.mapY)
  }

  def drawOutline(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    game.drawBoxMap(x1, y1, x2, y2, color)
  }

  def drawTextAtMobileUnit(u: Mobile, text: String, lineOffset: Int = 0): Unit = {
    game.drawTextMap(u.currentPositionNative.getX, u.currentPositionNative.getY + lineOffset * 10, text)
  }

  def drawTextAtStaticUnit(u: StaticallyPositioned, text: String, lineOffset: Int = 0): Unit = {
    game.drawTextMap(u.nativeMapPosition.getX, u.nativeMapPosition.getY + lineOffset * 10, text)
  }

  def indicateTarget(currentPosition: MapPosition, to: MapTilePosition): Unit = {
    game.drawLineMap(currentPosition.x, currentPosition.y, to.mapX, to.mapY, color)
    drawCircleAroundTile(to)
  }
  def drawCircleAroundTile(around: MapTilePosition): Unit = {
    game.drawCircleMap(around.mapX + tileSize / 2, around.mapX + tileSize / 2, tileSize / 2, color)
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

class LazyVal[T](gen: => T) {
  private var evaluated = false
  private var value: T  = _
  def get = {
    if (!evaluated)
      value = gen

    evaluated = true
    value
  }

  def invalidate(): Unit = {
    evaluated = false
    value = null.asInstanceOf[T]
  }
}

object LazyVal {
  def from[T](t: => T) = new LazyVal(t)
}

class FileStorageLazyVal[T](gen: => T, fileName: String) extends LazyVal(gen) {

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
  def from[T](gen: => T, unique: String) = new FileStorageLazyVal(gen, unique)

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
