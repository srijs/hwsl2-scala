class MutableHash private (m: SL2) {

  def this() = this(new SL2())

  def copy() = new MutableHash(m.copy())

  def withCopy_(f: MutableHash => Unit): MutableHash = {
    val c = copy()
    f(c)
    c
  }

  def append(buf: Array[Byte]) {
    m.mulBufR(buf)
  }

  def prepend(buf: Array[Byte]) {
    m.mulBufL(buf)
  }

  def foldAppend(bufs: Seq[Array[Byte]]) {
    bufs.foldLeft(()) { (_, buf) =>
      append(buf)
    }
  }

  def foldPrepend(bufs: Seq[Array[Byte]]) {
    bufs.foldRight(()) { (buf, _) =>
      prepend(buf)
    }
  }

}

class Hash private (mh: MutableHash) extends Immutable {

  def this() = this(new MutableHash())

  def append(buf: Array[Byte]): Hash =
    new Hash(mh.withCopy_ { _.append(buf) })

  def prepend(buf: Array[Byte]): Hash =
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