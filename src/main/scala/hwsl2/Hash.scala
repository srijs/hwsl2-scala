package hwsl2

import scalaz._

class Hash private (init: MutableHash) extends Immutable {

  private def mh = init

  def this() = this(new MutableHash())

  def canEqual(a: Any) = a.isInstanceOf[Hash]

  override def equals(that: Any): Boolean = that match {
    case that: Hash => that.canEqual(this) && mh.equals(that.mh)
    case _ => false
  }

  override def toString: String = mh.toString

  def compare(h: Hash) = mh.compare(h.mh)

  def concat(h: Hash) =
    new Hash(mh.concat(h.mh))

  def append(buf: Array[Byte]) =
    new Hash(mh.withCopy_ { _.append(buf) })

  def prepend(buf: Array[Byte]) =
    new Hash(mh.withCopy_ { _.prepend(buf) })

  def foldAppend[F[_]: Foldable](bufs: F[Array[Byte]]) =
    new Hash(mh.withCopy_ { _.foldAppend(bufs) })

  def foldPrepend[F[_]: Foldable](bufs: F[Array[Byte]]) =
    new Hash(mh.withCopy_ { _.foldPrepend(bufs) })

}

object Hash {

  def apply() = new Hash()
  def apply(buf: Array[Byte]): Hash = new Hash().append(buf)

  implicit val HashOrder: Order[Hash] = new Order[Hash] {
    def order(a: Hash, b: Hash) = a.compare(b) match {
      case 0          => Ordering.EQ
      case i if i > 0 => Ordering.GT
      case _          => Ordering.LT
    }
  }

  implicit val HashMonoid = new Monoid[Hash] {
    def zero: Hash = Hash()
    def append(a: Hash, b: => Hash): Hash = a.concat(b)
  }

}
