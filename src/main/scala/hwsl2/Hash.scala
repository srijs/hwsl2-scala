package hwsl2

class Hash private (mh: MutableHash) extends Immutable {

  def this() = this(new MutableHash())

  def canEqual(a: Any) = a.isInstanceOf[Hash]

  override def equals(that: Any): Boolean = that match {
    case that: Hash => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = 31 + mh.hashCode

  def append(buf: Array[Byte]) =
    new Hash(mh.withCopy_ { _.append(buf) })

  def prepend(buf: Array[Byte]) =
    new Hash(mh.withCopy_ { _.prepend(buf) })

  def foldAppend(bufs: Seq[Array[Byte]]) =
    new Hash(mh.withCopy_ { _.foldAppend(bufs) })

  def foldPrepend(bufs: Seq[Array[Byte]]) =
    new Hash(mh.withCopy_ { _.foldPrepend(bufs) })

}

object Hash {
  def unit = new Hash()

  def hash(buf: Array[Byte]) = new Hash().append(buf)
}
