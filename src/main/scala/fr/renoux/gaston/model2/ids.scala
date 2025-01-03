package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}
import scala.reflect.ClassTag
import scala.util.Random
import scala.annotation.targetName

opaque type Id >: Int = Int
opaque type SlotId >: Int <: Id = Int
opaque type TopicId >: Int <: Id = Int
opaque type PersonId >: Int <: Id = Int

object Id {
  extension (id: Id) {
    inline def value: Int = id
  }

  inline def None: Id = -1

  given [I <: Id]: Printable[I] with {
    extension (i: I) override def toPrettyString: String = i.toString
  }

  given [I <: Id]: Ordering[I] with {
    override def compare(x: I, y: I): Int = x.compareTo(y)
  }

}

object SlotId {
  inline def None: SlotId = -1
}

object TopicId {
  inline def None: TopicId = -1

  inline def Absent: TopicId = 0 // Topic for someone who isn't there
}

object PersonId {
  inline def None: PersonId = -1
}

opaque type Count[+I <: Id] >: Int = Int

object Count {
  extension [I >: Int <: Id](c: Count[I]) {
    inline def value: Int = c

    @targetName("CountInferior")
    inline def <(d: Count[I]) = c < d

    @targetName("CountSuperior")
    inline def >(d: Count[I]) = c > d

    @targetName("CountInferiorOrEqual")
    inline def <=(d: Count[I]) = c <= d

    @targetName("CountSuperiorOrEqual")
    inline def >=(d: Count[I]) = c >= d

    inline def range: Seq[I] = (0 until c)

    inline def foreach(inline f: I => Unit) = fastLoop(0, c)(f)

    inline def foreachTo(limit: I)(inline f: I => Unit) = fastLoop(0, limit + 1)(f)

    inline def foreachUntil(limit: I)(inline f: I => Unit) = fastLoop(0, limit)(f)

    inline def foreachWhile(inline f: I => Boolean) = {
      var i = 0
      while (i < c && f(i)) {
        i += 1
      }

    }

    inline def map[A](inline f: I => A): IndexedSeq[A] = (0 until c).map(f)

    // TODO inline this method
    /** Returns the first id in that count matching the condition. If none matches, returns Id.None. */
    def find(f: I => Boolean): I = {
      var i: I = 0
      var notFound = true
      var result: I = Id.None
      while (notFound && i < c) {
        if (f(i)) {
          result = i
          notFound = false
        }
        i += 1
      }
      result
    }

    /* TODO flatIndexes should be in the appropriate classes */
    inline def flatIndex(inline h: (Id | Int), i: I) = h * c + i

    /** Returns an array containing all values from the count, in a random order (using the Fisher-Yates shuffle) */
    inline def shuffled(using ct: ClassTag[I], rand: Random) = {
      val result: Array[I] = Array.tabulate(c)(identity)
      fastLoop(c - 1, 1, _- 1) { i =>
        val j = rand.nextInt(i + 1)
        val tmp = result(i)
        result(i) = result(j)
        result(j) = tmp
      }
      result
    }
  }

  /** A maximum count that won't overflow when it's summed with others */
  inline def maxCount[I >: Int <: Id]: Count[I] = 10_000

  val Zero: Count[Nothing] = 0

  given [I <: Id]: Printable[Count[I]] with {
    extension (c: Count[I]) override def toPrettyString: String = c.toString
  }
}

/** A count of entity that's guaranteed to contain all possible values. Therefore, it can be made implicit. Not a
  * super-type of Int to avoid weird implicit deductions. This must be given directly.
  */
opaque type CountAll[I <: Id] <: Count[I] = Int

object CountAll {
  def apply[I <: Id](c: Count[I]): CountAll[I] = c

  given [I <: Id]: Printable[CountAll[I]] with {
    extension (c: CountAll[I]) override def toPrettyString: String = c.toString
  }
}
