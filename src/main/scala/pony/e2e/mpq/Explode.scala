package pony.e2e.mpq

import scala.collection.mutable.ArrayBuffer

/** Decoder for PKWARE DCL "implode" data, ported from Mark Adler's public-domain blast.c. */
object Explode {
  private val MaxBits = 13

  private final class Huffman(val count: Array[Short], val symbol: Array[Short])

  private val LitLen = Array(
    11, 124, 8, 7, 28, 7, 188, 13, 76, 4, 10, 8, 12, 10, 12, 10, 8, 23, 8, 9, 7, 6, 7, 8, 7, 6,
    55, 8, 23, 24, 12, 11, 7, 9, 11, 12, 6, 7, 22, 5, 7, 24, 6, 11, 9, 6, 7, 22, 7, 11, 38, 7, 9, 8, 25, 11, 8, 11, 9,
    12, 8, 12, 5, 38, 5, 38, 5, 11, 7, 5, 6, 21, 6, 10, 53, 8, 7, 24, 10, 27, 44, 253, 253, 253, 252, 252, 252, 13, 12,
    45, 12, 45, 12, 61, 12, 45, 44, 173
  )
  private val LenLen  = Array(2, 35, 36, 53, 38, 23)
  private val DistLen = Array(2, 20, 53, 230, 247, 151, 248)
  private val Base    = Array(3, 2, 4, 5, 6, 7, 8, 9, 10, 12, 16, 24, 40, 72, 136, 264)
  private val Extra   = Array(0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8)

  private val litCode  = construct(LitLen, 256)
  private val lenCode  = construct(LenLen, 16)
  private val distCode = construct(DistLen, 64)

  def apply(input: Array[Byte]): Array[Byte] = new Decoder(input).run()

  /** Builds a canonical Huffman decoding table from blast's compact run-length form. */
  private def construct(rep: Array[Int], symbols: Int): Huffman = {
    val length = new Array[Int](symbols)
    var n      = 0
    rep.foreach { packed =>
      val len = packed & 15
      (0 to (packed >> 4)).foreach { _ =>
        length(n) = len
        n += 1
      }
    }
    val count = new Array[Short](MaxBits + 1)
    (0 until n).foreach(s => count(length(s)) = (count(length(s)) + 1).toShort)
    val offs = new Array[Int](MaxBits + 1)
    (1 until MaxBits).foreach(len => offs(len + 1) = offs(len) + count(len))
    val symbol = new Array[Short](symbols)
    (0 until n).foreach { s =>
      if (length(s) != 0) {
        symbol(offs(length(s))) = s.toShort
        offs(length(s)) += 1
      }
    }
    new Huffman(count, symbol)
  }

  private final class Decoder(input: Array[Byte]) {
    private var pos    = 0
    private var bitBuf = 0
    private var bitCnt = 0
    private val out    = new ArrayBuffer[Byte](input.length * 3)

    private def nextByte(): Int = {
      if (pos >= input.length) throw new IllegalArgumentException("Imploded data ends early")
      val b = input(pos) & 0xff
      pos += 1
      b
    }

    private def bits(need: Int): Int = {
      var value = bitBuf
      while (bitCnt < need) {
        value |= nextByte() << bitCnt
        bitCnt += 8
      }
      bitBuf = value >> need
      bitCnt -= need
      value & ((1 << need) - 1)
    }

    private def decode(h: Huffman): Int = {
      var buf    = bitBuf
      var left   = bitCnt
      var code   = 0
      var first  = 0
      var index  = 0
      var len    = 1
      var next   = 1
      var result = -1
      while (result < 0) {
        while (result < 0 && left > 0) {
          left -= 1
          code |= (buf & 1) ^ 1
          buf >>= 1
          val count = h.count(next)
          next += 1
          if (code < first + count) {
            bitBuf = buf
            bitCnt = (bitCnt - len) & 7
            result = h.symbol(index + (code - first)).toInt
          } else {
            index += count
            first += count
            first <<= 1
            code <<= 1
            len += 1
          }
        }
        if (result < 0) {
          left = (MaxBits + 1) - len
          if (left == 0) throw new IllegalArgumentException("Invalid imploded code")
          buf = nextByte()
          if (left > 8) left = 8
        }
      }
      result
    }

    def run(): Array[Byte] = {
      val literalsCoded = bits(8)
      if (literalsCoded > 1) throw new IllegalArgumentException(s"Invalid literal flag $literalsCoded")
      val dict = bits(8)
      if (dict < 4 || dict > 6) throw new IllegalArgumentException(s"Invalid dictionary size $dict")
      var done = false
      while (!done) {
        if (bits(1) == 1) {
          val lenSymbol = decode(lenCode)
          val len       = Base(lenSymbol) + bits(Extra(lenSymbol))
          if (len == 519) done = true
          else {
            val shift = if (len == 2) 2 else dict
            val dist  = (decode(distCode) << shift) + bits(shift) + 1
            if (dist > out.length) throw new IllegalArgumentException("Imploded distance too far back")
            val from = out.length - dist
            (0 until len).foreach(i => out += out(from + i))
          }
        } else {
          out += (if (literalsCoded == 1) decode(litCode) else bits(8)).toByte
        }
      }
      out.toArray
    }
  }
}
