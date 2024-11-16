package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import scala.collection.mutable


class SmallIdSetTest extends TestBase {
  val testAllInts: Seq[Int] = (0 until 64).toList
  val testAllIds: Seq[TopicId] = testAllInts
  val testOkInts: Seq[Int] = List(3, 8, 47, 63)
  val testOkIds: Seq[TopicId] = testOkInts
  val testKoInts: Seq[Int] = (testAllInts.toSet -- testOkInts).toList
  val testKoIds: Seq[TopicId] = testKoInts

  "Creation" - {
    "empty set" in {
      val set = SmallIdSet()
      set.underlying should be(0)
      SmallIdSet.empty[TopicId] should be(0)
    }

    "non-empty set" in {
      val set = SmallIdSet(testOkIds*)
      set.underlying should be(-9223231299366420216L)
    }

    "full set" in {
      val set = SmallIdSet(testAllIds*)
      set.underlying should be(-1L)
      SmallIdSet.full[TopicId] should be(-1L)
    }
  }

  "contains" - {
    "empty set" in {
      val set = SmallIdSet.empty[TopicId]
      testAllIds.foreach { id =>
        set.contains(id) should be(false)
        set(id) should be(false)
      }
    }

    "non-empty set" in {
      val set = SmallIdSet(testOkIds*)
      testOkIds.foreach { id =>
        set.contains(id) should be(true)
        set(id) should be(true)
      }
      testKoIds.foreach { id =>
        set.contains(id) should be(false)
        set(id) should be(false)
      }
    }

    "full set" in {
      val set = SmallIdSet.full[TopicId]
      testAllIds.foreach { id =>
        set.contains(id) should be(true)
        set(id) should be(true)
      }
    }
  }

  "added" - {
    "on empty set" in {
      val set = SmallIdSet.empty[TopicId]
      for (id <- testOkIds) {
        val s2 = set.inserted(id)
        s2.contains(id) should be(true)
        testAllIds.filterNot(_ == id).foreach { nid =>
          s2.contains(nid) should be(false)
        }
      }
    }

    "on non-empty set > non-existing value" in {
      val set: SmallIdSet[TopicId] = SmallIdSet(testOkIds*)
      for (id <- testKoIds) {
        val s2 = set.inserted(id)
        s2.contains(id) should be(true)
        testOkIds.foreach { nid =>
          s2.contains(nid) should be(true)
        }
        testKoIds.filterNot(_ == id).foreach { nid =>
          s2.contains(nid) should be(false)
        }
      }
    }

    "on non-empty set > existing value" in {
      val set: SmallIdSet[TopicId] = SmallIdSet(testOkIds*)
      for (id <- testOkIds) {
        val s2 = set.inserted(id)
        s2 should be(set)
        s2.contains(id) should be(true)
        testOkIds.filterNot(_ == id).foreach { nid =>
          s2.contains(nid) should be(true)
        }
        testKoIds.foreach { nid =>
          s2.contains(nid) should be(false)
        }
      }
    }

    "on full set" in {
      val set = SmallIdSet.full[TopicId]
      for (id <- testAllIds) {
        val s2 = set.inserted(id)
        s2 should be(set)
        s2.contains(id) should be(true)
        testAllIds.filterNot(_ == id).foreach { nid =>
          s2.contains(nid) should be(true)
        }
      }
    }
  }

  "removed" - {
    "on empty set" in {
      val set = SmallIdSet.empty[TopicId]
      for (id <- testOkIds) {
        val s2 = set.removed(id)
        s2 should be(set)
        s2.contains(id) should be(false)
        testAllIds.filterNot(_ == id).foreach { nid =>
          s2.contains(nid) should be(false)
        }
      }
    }

    "on non-empty set > non-existing value" in {
      val set: SmallIdSet[TopicId] = SmallIdSet(testOkIds*)
      for (id <- testKoIds) {
        val s2 = set.removed(id)
        s2 should be(set)
        s2.contains(id) should be(false)
        testOkIds.foreach { nid =>
          s2.contains(nid) should be(true)
        }
        testKoIds.filterNot(_ == id).foreach { nid =>
          s2.contains(nid) should be(false)
        }
      }
    }

    "on non-empty set > existing value" in {
      val set: SmallIdSet[TopicId] = SmallIdSet(testOkIds*)
      for (id <- testOkIds) {
        val s2 = set.removed(id)
        s2.contains(id) should be(false)
        testOkIds.filterNot(_ == id).foreach { nid =>
          s2.contains(nid) should be(true)
        }
        testKoIds.foreach { nid =>
          s2.contains(nid) should be(false)
        }
      }
    }

    "on full set" in {
      val set = SmallIdSet.full[TopicId]
      for (id <- testAllIds) {
        val s2 = set.removed(id)
        s2.contains(id) should be(false)
        testAllIds.filterNot(_ == id).foreach { nid =>
          s2.contains(nid) should be(true)
        }
      }
    }
  }

  "foreach" - {

    "on empty set" in {
      val set = SmallIdSet.empty[TopicId]
      val result = mutable.Set[TopicId]()
      set.foreach(result += _)
      result.isEmpty should be(true)
    }

    "on non-empty set" in {
      val set: SmallIdSet[TopicId] = SmallIdSet(testOkIds*)
      val result = mutable.Set[TopicId]()
      set.foreach(result += _)
      result.toSet should be(Set(testOkIds*))
    }

    "on full set" in {
      val set = SmallIdSet.full[TopicId]
      val result = mutable.Set[TopicId]()
      set.foreach(result += _)
      result.toSet should be(Set(testAllIds*))
    }
  }

  "mapSumToScore" - {
    "on empty set" in {
      val set = SmallIdSet.empty[TopicId]
      val result = set.mapSumToScore(tid => tid.value * 2)
      result should be(Score.Zero)
    }

    "on non-empty set" in {
      val set: SmallIdSet[TopicId] = SmallIdSet(testOkIds*)
      val result = set.mapSumToScore(tid => tid.value * 2)
      result should be(testOkInts.map(_ * 2).sum)
    }

    "on full set" in {
      val set = SmallIdSet.full[TopicId]
      val result = set.mapSumToScore(tid => tid.value * 2)
      result should be(testAllInts.map(_ * 2).sum)
    }
  }
}
