package fr.renoux.gaston

import fr.renoux.gaston.util.{BitMap, BitSet}

import scala.reflect.ClassTag

/** This package contains the Model, the runtime representation of the problem to solve. */
package object model {

  implicit final class SlotSetOps(val wrapped: Iterable[Slot]) extends AnyVal {
    @inline def toBitSet(implicit counts: Counts): BitSet[Slot] =
      BitSet.from[Slot](counts.slots)(wrapped)
  }

  implicit final class TopicSetOps(val wrapped: Iterable[Topic]) extends AnyVal {
    @inline def toBitSet(implicit counts: Counts): BitSet[Topic] =
      BitSet.from[Topic](counts.topics)(wrapped)
  }

  implicit final class PersonSetOps(val wrapped: Iterable[Person]) extends AnyVal {
    @inline def toBitSet(implicit counts: Counts): BitSet[Person] =
      BitSet.from[Person](counts.persons)(wrapped)
  }

  implicit final class SlotMapOps[A >: Null](val wrapped: Map[Slot, A]) extends AnyVal {
    @inline def toBitMap(implicit counts: Counts, tag: ClassTag[A]): BitMap[Slot, A] = toBitMap()

    @inline def toBitMap(default: A = null)(implicit counts: Counts, tag: ClassTag[A]): BitMap[Slot, A] = // scalastyle:ignore null
      BitMap.from[Slot, A](counts.slots, default)(wrapped)
  }

  implicit final class TopicMapOps[A >: Null](val wrapped: Map[Topic, A]) extends AnyVal {
    @inline def toBitMap(implicit counts: Counts, tag: ClassTag[A]): BitMap[Topic, A] = toBitMap()

    @inline def toBitMap(default: A = null)(implicit counts: Counts, tag: ClassTag[A]): BitMap[Topic, A] = // scalastyle:ignore null
      BitMap.from[Topic, A](counts.topics, default)(wrapped)
  }

  implicit final class PersonMapOps[A >: Null](val wrapped: Map[Person, A]) extends AnyVal {
    @inline def toBitMap(implicit counts: Counts, tag: ClassTag[A]): BitMap[Person, A] = toBitMap()

    @inline def toBitMap(default: A = null)(implicit counts: Counts, tag: ClassTag[A]): BitMap[Person, A] = // scalastyle:ignore null
      BitMap.from[Person, A](counts.persons, default)(wrapped)
  }

}
