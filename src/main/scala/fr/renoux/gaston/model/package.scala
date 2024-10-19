package fr.renoux.gaston

import fr.renoux.gaston.util.{ArrayMap, ArraySet}

import scala.reflect.ClassTag

/** This package contains the Model, the runtime representation of the problem to solve. */
package object model {

  implicit final class SlotSetOps(val wrapped: Iterable[Slot]) extends AnyVal {
    @inline def toArraySet(implicit counts: Counts): ArraySet[Slot] =
      ArraySet.from[Slot](counts.slots)(wrapped)
  }

  implicit final class TopicSetOps(val wrapped: Iterable[Topic]) extends AnyVal {
    @inline def toArraySet(implicit counts: Counts): ArraySet[Topic] =
      ArraySet.from[Topic](counts.topics)(wrapped)
  }

  implicit final class PersonSetOps(val wrapped: Iterable[Person]) extends AnyVal {
    @inline def toArraySet(implicit counts: Counts): ArraySet[Person] =
      ArraySet.from[Person](counts.persons)(wrapped)
  }

  implicit final class SlotMapOps[A >: Null](val wrapped: Map[Slot, A]) extends AnyVal {
    @inline def toArrayMap(implicit counts: Counts, tag: ClassTag[A]): ArrayMap[Slot, A] = toArrayMap()

    @inline def toArrayMap(default: A = null)(implicit counts: Counts, tag: ClassTag[A]): ArrayMap[Slot, A] =
      ArrayMap.from[Slot, A](counts.slots, default)(wrapped)
  }

  implicit final class TopicMapOps[A >: Null](val wrapped: Map[Topic, A]) extends AnyVal {
    @inline def toArrayMap(implicit counts: Counts, tag: ClassTag[A]): ArrayMap[Topic, A] = toArrayMap()

    @inline def toArrayMap(default: A = null)(implicit counts: Counts, tag: ClassTag[A]): ArrayMap[Topic, A] =
      ArrayMap.from[Topic, A](counts.topics, default)(wrapped)
  }

  implicit final class PersonMapOps[A >: Null](val wrapped: Map[Person, A]) extends AnyVal {
    @inline def toArrayMap(implicit counts: Counts, tag: ClassTag[A]): ArrayMap[Person, A] = toArrayMap()

    @inline def toArrayMap(default: A = null)(implicit counts: Counts, tag: ClassTag[A]): ArrayMap[Person, A] =
      ArrayMap.from[Person, A](counts.persons, default)(wrapped)
  }

}
