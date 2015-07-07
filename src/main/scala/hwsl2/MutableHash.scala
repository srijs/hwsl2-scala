package hwsl2

class MutableHash private (m: SL2) extends Mutable {

  def this() = this(new SL2())

  def copy() = new MutableHash(m.copy())

  def canEqual(a: Any) = a.isInstanceOf[MutableHash]

  override def equals(that: Any): Boolean = that match {
    case that: MutableHash => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = 31 + m.hashCode

  def withCopy_(f: MutableHash => Unit): MutableHash = {
    val c = copy(); f(c); c
  }

  def append(buf: Array[Byte]): Unit =
    m.mulBufR(buf)

  def prepend(buf: Array[Byte]): Unit =
    m.mulBufL(buf)

  def foldAppend(bufs: Seq[Array[Byte]]): Unit =
    bufs.foreach { buf => append(buf) }

  def foldPrepend(bufs: Seq[Array[Byte]]): Unit =
    bufs.foldRight(()) { (buf, _) => prepend(buf) } 
}
