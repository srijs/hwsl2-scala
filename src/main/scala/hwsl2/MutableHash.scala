package hwsl2

import scalaz._
import scalaz.Foldable._

import java.nio.ByteBuffer

class MutableHash private (init: HWSL2) extends Mutable with Equals {

  private def m = init

  def this() = {
    this(new HWSL2())
    reset
  }

  def reset = m.reset

  def copy(): MutableHash = {
    val mh = new MutableHash()
    mh.m.copy(m)
    mh
  }

  def canEqual(a: Any) = a.isInstanceOf[MutableHash]

  override def equals(that: Any): Boolean = that match {
    case that: MutableHash => that.canEqual(this) && m.eq(that.m)
    case _ => false
  }

  override def toString: String = m.serialize

  def withCopy_(f: MutableHash => Unit): MutableHash = {
    val c = copy(); f(c); c
  }

  def compare(mh: MutableHash): Int = m.cmp(mh.m)

  def concat(mh: MutableHash): MutableHash =
    new MutableHash(m.mul(mh.m))

  def append(buf: Array[Byte]): Unit =
    m.append(buf, buf.length)

  def prepend(buf: Array[Byte]): Unit =
    m.prepend(buf, buf.length)

  def foldAppend[F[_]: Foldable](bufs: F[Array[Byte]])(implicit f: Foldable[F]): Unit =
    f.foldLeft(bufs, ()) { (_, buf) => append(buf) }

  def foldPrepend[F[_]: Foldable](bufs: F[Array[Byte]])(implicit f: Foldable[F]): Unit =
    f.foldRight(bufs, ()) { (buf, _) => prepend(buf) }
}

object MutableHash {

  implicit val MutableHashOrder: Order[MutableHash] = new Order[MutableHash] {
    def order(a: MutableHash, b: MutableHash) = a.compare(b) match {
      case 0          => Ordering.EQ
      case i if i > 0 => Ordering.GT
      case _          => Ordering.LT
    }
  }

}
