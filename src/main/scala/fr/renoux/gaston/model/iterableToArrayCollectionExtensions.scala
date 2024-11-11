package fr.renoux.gaston.model

import fr.renoux.gaston.util.{ArrayMap, ArraySet, Count, Identified}

import scala.reflect.ClassTag

extension [A <: Identified](as: Iterable[A])(using count: Count[A]) {
  inline def toArraySet: ArraySet[A] =
    ArraySet.from[A](count.value)(as)
}

extension [A <: Identified, B >: Null: ClassTag](map: Map[A, B])(using count: Count[A]) {
  inline def toArrayMap: ArrayMap[A, B] = toArrayMap(null)

  inline def toArrayMap(default: B): ArrayMap[A, B] =
    ArrayMap.from[A, B](count.value, default)(map)
}
