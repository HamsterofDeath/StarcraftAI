package pony.e2e.chk

import java.nio.{ByteBuffer, ByteOrder}

/** The sections of a scenario.chk in file order; later duplicates win when the game reads them, as here. */
final case class ChkFile(sections: Vector[(String, Array[Byte])]) {
  def apply(name: String): Array[Byte] =
    sections.reverseIterator.find(_._1 == name).map(_._2).getOrElse(throw new NoSuchElementException(name))

  def get(name: String): Option[Array[Byte]] = sections.reverseIterator.find(_._1 == name).map(_._2)

  /**
    * The section as the game reads a tile layer: every duplicate written over the ones before it, a shorter one only
    * over its own length. Protected maps such as ICCup Lost_Temple follow a full MTXM with a partial one.
    */
  def overlaid(name: String): Array[Byte] =
    sections.collect { case (`name`, data) => data }.reduceLeftOption { (below, above) =>
      val merged = java.util.Arrays.copyOf(below, below.length max above.length)
      System.arraycopy(above, 0, merged, 0, above.length)
      merged
    }.getOrElse(throw new NoSuchElementException(name))

  def names: Vector[String] = sections.map(_._1)

  def bytes: Array[Byte] = {
    val out = new java.io.ByteArrayOutputStream()
    sections.foreach { case (name, data) =>
      out.write(name.getBytes("ISO-8859-1"))
      out.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(data.length).array())
      out.write(data)
    }
    out.toByteArray
  }
}

object ChkFile {
  def parse(data: Array[Byte]): ChkFile = {
    val buffer   = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val sections = Vector.newBuilder[(String, Array[Byte])]
    var position = 0
    while (position + 8 <= data.length) {
      val name = new String(data, position, 4, "ISO-8859-1")
      val size = buffer.getInt(position + 4)
      val from = position + 8
      // protected maps use negative or oversized lengths to confuse editors; the game clamps them
      val until = math.min(data.length, from + math.max(0, size))
      sections += name -> data.slice(from, until)
      position = if (size < 0) data.length else until
    }
    ChkFile(sections.result())
  }
}
