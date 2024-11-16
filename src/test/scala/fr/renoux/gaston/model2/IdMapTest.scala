package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase


class IdMapTest extends TestBase {
  val testAllInts: Seq[Int] = (0 until 64).toList
  val testAllIds: Seq[TopicId] = testAllInts
  val testOkInts: Seq[Int] = List(3, 8, 47, 63)
  val testOkIds: Seq[TopicId] = testOkInts
  val testKoInts: Seq[Int] = (testAllInts.toSet -- testOkInts).toList
  val testKoIds: Seq[TopicId] = testKoInts
  val testMapOk: Map[TopicId, String] = testOkIds.map { id =>
    id -> id.value.toHexString
  }.toMap
  val testMapAll: Map[TopicId, String] = testAllIds.map { id =>
    id -> id.value.toHexString
  }.toMap

  "Creation" - {
    "from" in {
      val map = IdMap.from(64)(testMapOk)
      map.toMap.filter(_._2 != null) should be(testMapOk)
    }

    "apply" in {
      val map = IdMap(testMapOk.toSeq*)
      map.toMap.filter(_._2 != null) should be(testMapOk)
    }
  }

  "apply" - {
    "read existing key" in {
      val map = IdMap.from(64)(testMapOk)
      testOkIds.foreach { id => map(id) should be(testMapOk(id)) }
    }

    "read non-existing key" in {
      val map = IdMap.from(64)(testMapOk)
      testKoIds.foreach { id =>
        map(id) should be(null)
      } // null is the default value for type String
    }
  }

  "mapToScore" in {
    val map = IdMap.from(64)(testMapAll)
    val result = map.mapToScore { (id, str) =>
      id.value + str.size
    }
    val expected = testAllIds.map[Score] { id => id.value + id.value.toHexString.size }
    result should be(expected)
  }
}
