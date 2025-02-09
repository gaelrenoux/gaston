package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase


class IdMapTest extends TestBase {
  given CountAll[SlotId] = CountAll[SlotId](64)

  given CountAll[TopicId] = CountAll[TopicId](64)

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
      val map = IdMap.from(testMapOk)
      map.toMap.filter(_._2 != null) should be(testMapOk)
    }

    "apply" in {
      val map = IdMap(testMapOk.toSeq *)
      map.toMap.filter(_._2 != null) should be(testMapOk)
    }

    "fill" in {
      val map = IdMap.fill[TopicId, String]("hello")
      val expected = Seq.tabulate(64)(_ -> "hello").toMap
      map.toMap should be(expected)
    }

    "tabulate" in {
      val map = IdMap.tabulate[TopicId, String](id => testMapAll(id.value))
      map.toMap should be(testMapAll)
    }
  }

  "apply" - {
    "read existing key" in {
      val map = IdMap.from(testMapOk)
      testOkIds.foreach { id => map(id) should be(testMapOk(id)) }
    }

    "read non-existing key" in {
      val map = IdMap.from(testMapOk)
      testKoIds.foreach { id =>
        map(id) should be(null)
      } // null is the default value for type String
    }
  }

  "mapToScore" in {
    val map = IdMap.from(testMapOk)
    val result = map.mapToScore { (id, str) =>
      if (str == null) 0
      else id.value + str.size
    }
    val expected = testOkIds.map[(TopicId, Score)] { id => id -> (id.value + id.value.toHexString.size) }
    result should be(IdMap.from(expected))
  }

  "toMap" in {
    val map = IdMap.from(testMapAll)
    map.toMap should be(testMapAll)
  }

  "toReverseMap" in {
    val map = IdMap.from(testMapAll)
    map.toReverseMap should be(testMapAll.map(_.swap))
  }

  "sortedValues" in {
    val map = IdMap.from(testAllInts.map[(TopicId, Score)](i => (i, 10 * i)))
    map.sortedValues should be(testAllInts.map(_ * 10).sorted)
  }

  "reduceValues" in {
    val map = IdMap.from(testMapAll)
    map.reduceValues(_ + _) should be(testMapAll.toSeq.sortBy(_._1.value).map(_._2).reduceLeft(_ + _))
  }

  "transpose (Id-to-Id map)" in {
    val map = IdMap.from[SlotId, TopicId](
      Seq.tabulate(64) { id => id -> id % 4 }
    )
    val expected = IdMap.from[TopicId, SmallIdSet[SlotId]](
      Seq.tabulate(4) { id => id -> SmallIdSet((0 until 16).map(_ * 4 + id)) }
    )
    map.transpose should be(expected)
  }

  "transpose (Id-to-IdSet map)" in {
    val map = IdMap.from[SlotId, SmallIdSet[TopicId]](
      Seq.tabulate(64)(id =>
        if (id == 0) id -> SmallIdSet(63, 0)
        else id -> SmallIdSet(id, id - 1)
      )
    )
    val expected = IdMap.from[TopicId, SmallIdSet[SlotId]](
      Seq.tabulate(64)(id =>
        if (id == 63) id -> SmallIdSet(63, 0)
        else id -> SmallIdSet(id, id + 1)
      )
    )
    map.transpose should be(expected)
  }

  "copy" in {
    val map = IdMap.from(testMapAll)
    val map2 = map.copy()
    map2.toMap should be(map.toMap)

    map2(42) = ""
    map2(42) should be("")
    map(42) should be("2a")
  }

}
