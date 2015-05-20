package hwsl2

case class Gf2p127(lo: Long, hi: Long)

object Gf2p127 {

  def zero = new Gf2p127(0, 0)
  def one = new Gf2p127(1, 0)

  def minmax(bit: Int) = {
    new Gf2p127(0 - bit, 0 - bit)
  }

  def add(a: Gf2p127, b: Gf2p127): Gf2p127 = {
    new Gf2p127(a.lo ^ b.lo, a.hi ^ b.hi)
  }

  def mask(a: Gf2p127, b: Gf2p127): Gf2p127 = {
    new Gf2p127(a.lo & b.lo, a.hi & b.hi)
  }

  def mul10(a: Gf2p127): Gf2p127 = {
    val sl_lo = a.lo << 1
    val sl_hi = a.hi << 1
    val carry = a.lo >>> 63
    val overflow = sl_hi >>> 63
    val carried = sl_hi ^ carry
    val final_lo = sl_lo ^ overflow ^ (overflow << 63)
    val final_hi = carried ^ (overflow << 63)
    new Gf2p127(final_lo, final_hi)
  }

}
