package pony

import java.io._
import java.util.zip.{Deflater, ZipEntry, ZipInputStream, ZipOutputStream}

import org.apache.commons.io.FileUtils

import scala.util.Try

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
    val zi        = new ZipInputStream(new ByteArrayInputStream(bytes))
    val nextEntry = zi.getNextEntry
    val os        = new ObjectInputStream(new BufferedInputStream(zi))
    Try {
      val ret = os.readObject().asInstanceOf[T]
      os.close()
      ret
    }.toOption
  }

  private def file = FileStorageLazyVal.fileByName(fileName)

  private def toZippedBytes(t: T): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val o     = new ObjectOutputStream(bytes)
    o.writeObject(t)
    val data   = bytes.toByteArray
    val zipped = new ByteArrayOutputStream()
    val zo     = new ZipOutputStream(zipped)
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
