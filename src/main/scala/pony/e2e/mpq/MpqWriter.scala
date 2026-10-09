package pony.e2e.mpq

import java.nio.{ByteBuffer, ByteOrder}

/** Writes an MPQ v1 archive with uncompressed, unencrypted files, which StarCraft loads as a map. */
object MpqWriter {
  private val HeaderSize = 32

  def write(files: Seq[(String, Array[Byte])]): Array[Byte] = {
    val hashEntries = Iterator.iterate(16)(_ * 2).find(_ >= files.size * 2).get
    val dataSize = files.map(_._2.length).sum
    val hashOffset = HeaderSize + dataSize
    val blockOffset = hashOffset + hashEntries * 16
    val total = blockOffset + files.size * 16

    val out = ByteBuffer.allocate(total).order(ByteOrder.LITTLE_ENDIAN)
    out.put("MPQ".getBytes("ASCII")).put(0x1a.toByte)
    out.putInt(HeaderSize).putInt(total).putShort(0.toShort).putShort(3.toShort)
    out.putInt(hashOffset).putInt(blockOffset).putInt(hashEntries).putInt(files.size)

    val blocks = ByteBuffer.allocate(files.size * 16).order(ByteOrder.LITTLE_ENDIAN)
    val hashes = ByteBuffer.allocate(hashEntries * 16).order(ByteOrder.LITTLE_ENDIAN)
    (0 until hashEntries).foreach { i =>
      hashes.putInt(i * 16, -1).putInt(i * 16 + 4, -1).putInt(i * 16 + 8, -1).putInt(i * 16 + 12, -1)
    }
    var position = HeaderSize
    files.zipWithIndex.foreach { case ((path, data), index) =>
      out.put(data)
      blocks.putInt(position).putInt(data.length).putInt(data.length).putInt(MpqArchive.Exists)
      position += data.length
      var slot = MpqCrypto.hash(path, MpqCrypto.TableOffset) & (hashEntries - 1)
      while (hashes.getInt(slot * 16 + 12) != -1) slot = (slot + 1) % hashEntries
      hashes.putInt(slot * 16, MpqCrypto.hash(path, MpqCrypto.NameA))
      hashes.putInt(slot * 16 + 4, MpqCrypto.hash(path, MpqCrypto.NameB))
      hashes.putInt(slot * 16 + 8, 0)
      hashes.putInt(slot * 16 + 12, index)
    }
    out.put(MpqCrypto.encrypt(hashes.array(), MpqCrypto.hash("(hash table)", MpqCrypto.FileKey)))
    out.put(MpqCrypto.encrypt(blocks.array(), MpqCrypto.hash("(block table)", MpqCrypto.FileKey)))
    out.array()
  }
}
