package pony.e2e.mpq

import java.nio.{ByteBuffer, ByteOrder}
import java.util.zip.Inflater

/** Read-only access to the files of an MPQ v1 archive, enough for StarCraft maps and data archives. */
final class MpqArchive(bytes: Array[Byte]) {
  import MpqArchive.*

  private val start = (0 until bytes.length by 512).find { offset =>
    offset + 32 <= bytes.length && bytes(offset) == 'M' && bytes(offset + 1) == 'P' && bytes(offset + 2) == 'Q' &&
    bytes(offset + 3) == 0x1a
  }.getOrElse(throw new IllegalArgumentException("No MPQ header"))

  private val header          = le(bytes, start, 32)
  private val sectorSize      = 512 << header.getShort(14)
  private val hashTableOffset = header.getInt(16)
  private val blockOffset     = header.getInt(20)
  private val hashEntries     = header.getInt(24)
  private val blockEntries    = header.getInt(28)

  private val hashTable = le(table(hashTableOffset, hashEntries, MpqCrypto.hash("(hash table)", MpqCrypto.FileKey)), 0,
    hashEntries * 16)
  private val blockTable = le(table(blockOffset, blockEntries, MpqCrypto.hash("(block table)", MpqCrypto.FileKey)), 0,
    blockEntries * 16)

  def contains(path: String): Boolean = blockIndex(path).isDefined

  def read(path: String): Array[Byte] = {
    val block = blockIndex(path).getOrElse(throw new NoSuchElementException(path))
    val filePos = blockTable.getInt(block * 16)
    val packedSize = blockTable.getInt(block * 16 + 4)
    val fileSize = blockTable.getInt(block * 16 + 8)
    val flags = blockTable.getInt(block * 16 + 12)
    val key = {
      val base = MpqCrypto.fileKey(path)
      if ((flags & FixKey) != 0) (base + filePos) ^ fileSize else base
    }
    val encrypted = (flags & Encrypted) != 0
    val data = bytes.slice(start + filePos, start + filePos + packedSize)
    val packed = (flags & (Implode | Compress)) != 0
    if ((flags & SingleUnit) != 0 || !packed) {
      val plain = if (encrypted) MpqCrypto.decrypt(data, key) else data
      if (packed && packedSize < fileSize) unpack(plain, flags, fileSize) else plain.take(fileSize)
    } else {
      val sectors = (fileSize + sectorSize - 1) / sectorSize
      val offsetBytes = data.take((sectors + 1) * 4)
      val offsets = le(if (encrypted) MpqCrypto.decrypt(offsetBytes, key - 1) else offsetBytes, 0, (sectors + 1) * 4)
      val out = new java.io.ByteArrayOutputStream(fileSize)
      (0 until sectors).foreach { i =>
        val from = offsets.getInt(i * 4)
        val until = offsets.getInt(i * 4 + 4)
        val raw = data.slice(from, until)
        val sector = if (encrypted) MpqCrypto.decrypt(raw, key + i) else raw
        val expected = math.min(sectorSize, fileSize - i * sectorSize)
        out.write(if (sector.length < expected) unpack(sector, flags, expected) else sector)
      }
      out.toByteArray
    }
  }

  private def table(offset: Int, entries: Int, key: Int) =
    MpqCrypto.decrypt(bytes.slice(start + offset, start + offset + entries * 16), key)

  private def blockIndex(path: String): Option[Int] = {
    val nameA = MpqCrypto.hash(path, MpqCrypto.NameA)
    val nameB = MpqCrypto.hash(path, MpqCrypto.NameB)
    val first = MpqCrypto.hash(path, MpqCrypto.TableOffset) & (hashEntries - 1)
    Iterator.iterate(first)(i => (i + 1) % hashEntries).take(hashEntries).map(_ * 16)
      .takeWhile(entry => hashTable.getInt(entry + 12) != EmptySlot)
      .find(entry => hashTable.getInt(entry) == nameA && hashTable.getInt(entry + 4) == nameB &&
        hashTable.getInt(entry + 12) != DeletedSlot)
      .map(entry => hashTable.getInt(entry + 12))
  }

  private def unpack(data: Array[Byte], flags: Int, size: Int): Array[Byte] = {
    if ((flags & Implode) != 0) Explode(data)
    else {
      val method = data(0) & 0xff
      val payload = data.drop(1)
      method match {
        case 0x08 => Explode(payload)
        case 0x02 =>
          val inflater = new Inflater()
          inflater.setInput(payload)
          val out = new Array[Byte](size)
          val n = inflater.inflate(out)
          inflater.end()
          out.take(n)
        case other => throw new UnsupportedOperationException(f"MPQ compression 0x$other%02x")
      }
    }
  }
}

object MpqArchive {
  val Implode    = 0x00000100
  val Compress   = 0x00000200
  val Encrypted  = 0x00010000
  val FixKey     = 0x00020000
  val SingleUnit = 0x01000000
  val Exists     = 0x80000000

  private val EmptySlot   = 0xffffffff
  private val DeletedSlot = 0xfffffffe

  def open(path: java.nio.file.Path): MpqArchive = new MpqArchive(java.nio.file.Files.readAllBytes(path))

  private[mpq] def le(data: Array[Byte], offset: Int, length: Int) =
    ByteBuffer.wrap(data, offset, length).slice().order(ByteOrder.LITTLE_ENDIAN)
}
