package hwsl2

class SL2(
  var a: Gf2p127 = Gf2p127.one,
  var b: Gf2p127 = Gf2p127.zero,
  var c: Gf2p127 = Gf2p127.zero,
  var d: Gf2p127 = Gf2p127.one
) {

  def copy() = {
    new SL2(a, b, c, d)
  }

  def canEqual(a: Any) = a.isInstanceOf[SL2]

  override def equals(that: Any): Boolean = that match {
    case that: SL2 => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + a.hashCode
    result = prime * result + b.hashCode
    result = prime * result + c.hashCode
    result = prime * result + d.hashCode
    return result
  }

  def mulBitL(bit: Int) {
    val bits = Gf2p127.minmax(bit)
    val m10_ = c
    val m11_ = d
    c = Gf2p127.add(a, Gf2p127.mask(c, bits))
    d = Gf2p127.add(b, Gf2p127.mask(d, bits))
    a = Gf2p127.add(m10_, Gf2p127.mul10(c))
    b = Gf2p127.add(m11_, Gf2p127.mul10(d))
  }

  def mulBitR(bit: Int) {
    val bits = Gf2p127.minmax(bit)
    val m00_ = a
    val m10_ = c
    a = Gf2p127.add(b, Gf2p127.mul10(a))
    c = Gf2p127.add(d, Gf2p127.mul10(c))
    b = Gf2p127.add(m00_, Gf2p127.mask(a, bits))
    d = Gf2p127.add(m10_, Gf2p127.mask(c, bits))
  }

  def mulByteL(byte: Byte) {
    mulBitL((byte >>> 0) & 1)
    mulBitL((byte >>> 1) & 1)
    mulBitL((byte >>> 2) & 1)
    mulBitL((byte >>> 3) & 1)
    mulBitL((byte >>> 4) & 1)
    mulBitL((byte >>> 5) & 1)
    mulBitL((byte >>> 6) & 1)
    mulBitL((byte >>> 7) & 1)
  }

  def mulByteR(byte: Byte) {
    mulBitR((byte >>> 7) & 1)
    mulBitR((byte >>> 6) & 1)
    mulBitR((byte >>> 5) & 1)
    mulBitR((byte >>> 4) & 1)
    mulBitR((byte >>> 3) & 1)
    mulBitR((byte >>> 2) & 1)
    mulBitR((byte >>> 1) & 1)
    mulBitR((byte >>> 0) & 1)
  }

  def mulBufL(buf: Array[Byte]) {
    for (i <- buf.length-1 to 0 by -1) {
      mulByteL(buf(i))
    }
  }
 
  def mulBufR(buf: Array[Byte]) {
    for (i <- 0 until buf.length) {
      mulByteR(buf(i))
    }
  }

}
