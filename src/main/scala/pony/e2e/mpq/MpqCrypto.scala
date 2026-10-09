package pony.e2e.mpq

import java.nio.{ByteBuffer, ByteOrder}

/** Storm's hashing and table/file encryption. */
object MpqCrypto {
  val TableOffset = 0
  val NameA       = 1
  val NameB       = 2
  val FileKey     = 3

  private val cryptTable: Array[Int] = {
    val table = new Array[Int](0x500)
    var seed  = 0x00100001L
    (0 until 0x100).foreach { index1 =>
      var index2 = index1
      (0 until 5).foreach { _ =>
        seed = (seed * 125 + 3) % 0x2aaaab
        val high = (seed & 0xffff) << 16
        seed = (seed * 125 + 3) % 0x2aaaab
        val low = seed & 0xffff
        table(index2) = (high | low).toInt
        index2 += 0x100
      }
    }
    table
  }

  def hash(text: String, hashType: Int): Int = {
    var seed1 = 0x7fed7fed
    var seed2 = 0xeeeeeeee
    text.toUpperCase.foreach { c =>
      val ch = c.toInt & 0xff
      seed1 = cryptTable((hashType << 8) + ch) ^ (seed1 + seed2)
      seed2 = ch + seed1 + seed2 + (seed2 << 5) + 3
    }
    seed1
  }

  /** The key of a stored file, from its name without the directory part. */
  def fileKey(path: String): Int = hash(path.substring(path.lastIndexOf('\\') + 1), FileKey)

  def decrypt(data: Array[Byte], key: Int): Array[Byte] = transform(data, key, encrypting = false)

  def encrypt(data: Array[Byte], key: Int): Array[Byte] = transform(data, key, encrypting = true)

  /** Works on whole little-endian words; trailing bytes stay as they are, like Storm. */
  private def transform(data: Array[Byte], startKey: Int, encrypting: Boolean): Array[Byte] = {
    val result = data.clone()
    val words  = ByteBuffer.wrap(result).order(ByteOrder.LITTLE_ENDIAN)
    var key    = startKey
    var seed   = 0xeeeeeeee
    (0 until data.length / 4).foreach { i =>
      seed += cryptTable(0x400 + (key & 0xff))
      val input  = words.getInt(i * 4)
      val output = input ^ (key + seed)
      val plain  = if (encrypting) input else output
      key = ((~key << 0x15) + 0x11111111) | (key >>> 0x0b)
      seed = plain + seed + (seed << 5) + 3
      words.putInt(i * 4, output)
    }
    result
  }
}
