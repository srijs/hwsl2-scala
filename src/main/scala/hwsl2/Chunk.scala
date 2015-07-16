package hwsl2

import scalaz._
import scalaz.syntax.monoid._

case class Chunk(hash: Hash, bytes: Array[Byte])

object Chunk {

  def apply() = new Chunk(Hash(), Array())
  def apply(b: Array[Byte]) = new Chunk(Hash(b), b)

}
