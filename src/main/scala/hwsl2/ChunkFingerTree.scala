package hwsl2

import scalaz._
import scalaz.syntax.monoid._

object ChunkFingerTree {

  case class Measure(hash: Hash, length: Long)

  object Measure {

    implicit def monoid = new Monoid[Measure] {
      def zero: Measure = Measure(Hash(), 0)
      def append(a: Measure, b: => Measure): Measure =
        Measure(a.hash |+| b.hash, a.length + b.length)
    }

    implicit def reducer: Reducer[Chunk, Measure] = Reducer(
      c => Measure(c.hash, c.bytes.length),
      c => a => Measure(c.hash |+| a.hash, a.length + c.bytes.length),
      a => c => Measure(a.hash |+| c.hash, a.length + c.bytes.length)
    )

  }

  def apply() = FingerTree.empty(Measure.reducer)

}
