package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.util.{Count as _, *}
import scala.collection.mutable
import scala.util.Random


class SmallIdSetTest extends TestBase {

  /* OK ids and added id must be distinct */
  /* Make sure there's always at least one multiple of 3 and one non-multilpe of 3 */
  "With a max count" - {
    behave like behaviorFor(List(3, 8, 24, 47, 61), List(12, 19, 41))(using CountAll(64))
  }
  "With a lower count" - {
    behave like behaviorFor(List(4, 5, 9), List(3, 6, 7))(using CountAll(12))
  }
  "Having both min and max value" - {
    behave like behaviorFor(List(0, 34, 63), List(1, 15, 62))(using CountAll(64))
  }

  def behaviorFor(okIds: Seq[TopicId], addedIds: Seq[TopicId])(using countAll: CountAll[TopicId]): Unit = {
    val allIds: Seq[TopicId] = (0 until countAll.value).toList
    val koIds: Seq[TopicId] = (allIds.toSet -- okIds).toList.sorted

    val testOkTopicIds = okIds
    val testAllTopicIds = allIds


    "Creation" - {
      "empty set" in {
        val set = SmallIdSet.empty[TopicId]
        set.underlying should be(0)
        set.isEmpty should be(true)
        set.toSet.isEmpty should be(true)
      }

      "set created from empty" in {
        val set = SmallIdSet[TopicId]()
        set.underlying should be(0)
        set.isEmpty should be(true)
        set.toSet.isEmpty should be(true)
      }

      "non-empty set" in {
        val set = SmallIdSet(okIds *)
        set.isEmpty should be(false)
        set.toSet should be(okIds.toSet)
      }

      "full set" in {
        val set = SmallIdSet(allIds *)
        set.isEmpty should be(false)
        set.toSet should be(allIds.toSet)
      }

      "set created from full" in {
        val set = SmallIdSet(allIds *)
        set.isEmpty should be(false)
        set.toSet should be(allIds.toSet)
      }
    }

    "contains" - {
      "empty set" in {
        val set = SmallIdSet.empty[TopicId]
        allIds.foreach { id =>
          set.contains(id) should be(false)
          set(id) should be(false)
        }
      }

      "non-empty set" in {
        val set = SmallIdSet(okIds *)
        okIds.foreach { id =>
          set.contains(id) should be(true)
          set(id) should be(true)
        }
        koIds.foreach { id =>
          set.contains(id) should be(false)
          set(id) should be(false)
        }
      }

      "full set" in {
        val set = SmallIdSet.full[TopicId]
        allIds.foreach { id =>
          set.contains(id) should be(true)
          set(id) should be(true)
        }
      }
    }

    "containsAll" - {
      "empty set" in {
        val set = SmallIdSet.empty[TopicId]
        allIds.cross(allIds).foreach { (i, j) =>
          set.containsAll(i, j) should be(false)
        }
      }

      "small set" in {
        val set = SmallIdSet[TopicId](4, 8)
        set.containsAll(4, 8) should be(true)
        set.containsAll(8, 4) should be(true)
        set.containsAll(6, 8) should be(false)
        set.containsAll(4, 9) should be(false)
        set.containsAll(43, 9) should be(false)
      }

      "non-empty set" in {
        val set = SmallIdSet(okIds *)
        okIds.cross(okIds).foreach { (i, j) =>
          set.containsAll(i, j) should be(true)
        }
        koIds.cross(okIds).foreach { (i, j) =>
          set.containsAll(i, j) should be(false)
        }
        okIds.cross(koIds).foreach { (i, j) =>
          set.containsAll(i, j) should be(false)
        }
        koIds.cross(koIds).foreach { (i, j) =>
          set.containsAll(i, j) should be(false)
        }
      }

      "full set" in {
        val set = SmallIdSet.full[TopicId]
        allIds.cross(allIds).foreach { (i, j) =>
          set.containsAll(i, j) should be(true)
        }
      }
    }

    "size" - {
      "empty set" in {
        val set = SmallIdSet.empty[TopicId]
        set.size should be(0)
      }

      "non-empty set" in {
        val set = SmallIdSet(okIds *)
        set.size should be(okIds.size)
      }

      "full set" in {
        val set = SmallIdSet.full[TopicId]
        set.size should be(countAll.value)
      }
    }

    "min" - {
      "empty set" in {
        val set = SmallIdSet.empty[TopicId]
        set.min should be(TopicId.None)
      }

      "non-empty set" in {
        val set = SmallIdSet[TopicId](okIds *)
        set.min.value should be(okIds.min)
      }

      "full set" in {
        val set = SmallIdSet.full[TopicId]
        set.min.value should be(0)
      }
    }

    "max" - {
      "empty set" in {
        val set = SmallIdSet.empty[TopicId]
        set.max should be(TopicId.None)
      }

      "non-empty set" in {
        val set = SmallIdSet[TopicId](okIds *)
        set.max.value should be(okIds.max)
      }

      "full set" in {
        val set = SmallIdSet.full[TopicId]
        set.max.value should be(countAll.value - 1)
      }
    }

    "plus" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        for (id <- okIds) {
          val s2 = set + id
          s2.contains(id) should be(true)
          allIds.filterNot(_ == id).foreach { nid =>
            s2.contains(nid) should be(false)
          }
        }
      }

      "on non-empty set > non-existing value" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        for (id <- koIds) {
          val s2 = set + id
          s2.contains(id) should be(true)
          okIds.foreach { nid =>
            s2.contains(nid) should be(true)
          }
          koIds.filterNot(_ == id).foreach { nid =>
            s2.contains(nid) should be(false)
          }
        }
      }

      "on non-empty set > existing value" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        for (id <- okIds) {
          val s2 = set + id
          s2 should be(set)
          s2.contains(id) should be(true)
          okIds.filterNot(_ == id).foreach { nid =>
            s2.contains(nid) should be(true)
          }
          koIds.foreach { nid =>
            s2.contains(nid) should be(false)
          }
        }
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        for (id <- allIds) {
          val s2 = set + id
          s2 should be(set)
          s2.contains(id) should be(true)
          allIds.filterNot(_ == id).foreach { nid =>
            s2.contains(nid) should be(true)
          }
        }
      }
    }

    "minus" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        for (id <- okIds) {
          val s2 = set - id
          s2 should be(set)
          s2.contains(id) should be(false)
          allIds.filterNot(_ == id).foreach { nid =>
            s2.contains(nid) should be(false)
          }
        }
      }

      "on non-empty set > non-existing value" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        for (id <- koIds) {
          val s2 = set - id
          s2 should be(set)
          s2.contains(id) should be(false)
          okIds.foreach { nid =>
            s2.contains(nid) should be(true)
          }
          koIds.filterNot(_ == id).foreach { nid =>
            s2.contains(nid) should be(false)
          }
        }
      }

      "on non-empty set > existing value" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        for (id <- okIds) {
          val s2 = set - id
          s2.contains(id) should be(false)
          okIds.filterNot(_ == id).foreach { nid =>
            s2.contains(nid) should be(true)
          }
          koIds.foreach { nid =>
            s2.contains(nid) should be(false)
          }
        }
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        for (id <- allIds) {
          val s2 = set - id
          s2.contains(id) should be(false)
          allIds.filterNot(_ == id).foreach { nid =>
            s2.contains(nid) should be(true)
          }
        }
      }
    }

    "plusPlus Iterable" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        val s2 = set ++ okIds
        okIds.foreach { id => s2.contains(id) should be(true) }
        koIds.foreach { id => s2.contains(id) should be(false) }
      }

      "on non-empty set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        val s2 = set ++ addedIds
        okIds.foreach { id => s2.contains(id) should be(true) }
        addedIds.foreach { id => s2.contains(id) should be(true) }
        koIds.filterNot(addedIds.contains).foreach { id => s2.contains(id) should be(false) }
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        val s2 = set ++ okIds
        allIds.foreach { id => s2.contains(id) should be(true) }
      }
    }

    "plusPlus SmallIdSet" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        val s2 = set ++ SmallIdSet(okIds *)
        okIds.foreach { id => s2.contains(id) should be(true) }
        koIds.foreach { id => s2.contains(id) should be(false) }
      }

      "on non-empty set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        val s2 = set ++ SmallIdSet(addedIds)
        okIds.foreach { id => s2.contains(id) should be(true) }
        addedIds.foreach { id => s2.contains(id) should be(true) }
        koIds.filterNot(addedIds.contains).foreach { id => s2.contains(id) should be(false) }
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        val s2 = set ++ SmallIdSet(okIds *)
        allIds.foreach { id => s2.contains(id) should be(true) }
      }
    }

    "minusMinus Iterable" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        val s2 = set -- okIds
        allIds.foreach { id => s2.contains(id) should be(false) }
      }

      "on non-empty set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        val removedIds: Set[TopicId] = okIds.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).toSet
        val s2 = set -- removedIds
        koIds.foreach { id => s2.contains(id) should be(false) }
        removedIds.foreach { id => s2.contains(id) should be(false) }
        okIds.filterNot(removedIds).foreach { id => s2.contains(id) should be(true) }
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        val s2 = set -- okIds
        okIds.foreach { id => s2.contains(id) should be(false) }
        koIds.foreach { id => s2.contains(id) should be(true) }
      }
    }

    "minusMinus SmallIdSet" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        val s2 = set -- SmallIdSet(okIds *)
        allIds.foreach { id => s2.contains(id) should be(false) }
      }

      "on non-empty set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        val removedIds: Set[TopicId] = okIds.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).toSet
        val s2 = set -- SmallIdSet(removedIds)
        koIds.foreach { id => s2.contains(id) should be(false) }
        removedIds.foreach { id => s2.contains(id) should be(false) }
        okIds.filterNot(removedIds).foreach { id => s2.contains(id) should be(true) }
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        val s2 = set -- SmallIdSet(okIds *)
        okIds.foreach { id => s2.contains(id) should be(false) }
        koIds.foreach { id => s2.contains(id) should be(true) }
      }
    }

    "inversed" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        set.inversed should be(SmallIdSet.full[TopicId])
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        set.inversed should be(SmallIdSet.empty[TopicId])
      }

      "on non-empty set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(okIds *)
        set.inversed should be(SmallIdSet(koIds *))
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
        val set: SmallIdSet[TopicId] = SmallIdSet(testOkTopicIds *)
        val result = mutable.Set[TopicId]()
        set.foreach(result += _)
        result.toSet should be(Set(testOkTopicIds *))
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        val result = mutable.Set[TopicId]()
        set.foreach(result += _)
        result.toSet should be(Set(testAllTopicIds *))
      }
    }

    "foreachWhile" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        val result = mutable.Set[TopicId]()
        set.foreachWhile { i =>
          if (i.value < 20) {
            result += i
            true
          } else false
        }
        result.isEmpty should be(true)
      }

      "on non-empty set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(testOkTopicIds *)
        val result = mutable.Set[TopicId]()
        set.foreachWhile { i =>
          if (i.value < 20) {
            result += i
            true
          } else false
        }
        result.toSet should be(Set(testOkTopicIds *).filter(_.value < 20))
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        val result = mutable.Set[TopicId]()
        set.foreachWhile { i =>
          if (i.value < 20) {
            result += i
            true
          } else false
        }
        result.toSet should be(Set(testAllTopicIds *).filter(_.value < 20))
      }

      "precise split" in {
        val set = SmallIdSet[TopicId](1, 2, 3)
        val result = mutable.Set[TopicId]()
        set.foreachWhile { i =>
          if (i.value < 2) {
            result += i
            true
          } else false
        }
        result.toSet should be(Set(1))
      }
    }

    "foreachPair" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        val result = mutable.Set[(TopicId, TopicId)]()
        set.foreachPair { (i, j) => result += i -> j }
        result.isEmpty should be(true)
      }

      "on small set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(1, 2)
        val result = mutable.Set[(TopicId, TopicId)]()
        set.foreachPair { (i: TopicId, j: TopicId) => result += i -> j }
        result.toSet should be(Set(2 -> 1))
      }

      "on non-empty set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(testOkTopicIds *)
        val result = mutable.Set[(TopicId, TopicId)]()
        set.foreachPair { (i: TopicId, j: TopicId) => result += i -> j }
        result.toSet should be(testOkTopicIds.cross(testOkTopicIds).toSet.filter(p => p._1.value > p._2.value))
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        val result = mutable.Set[(TopicId, TopicId)]()
        set.foreachPair { (i, j) => result += i -> j }
        result.toSet should be(testAllTopicIds.cross(testAllTopicIds).toSet.filter(p => p._1.value > p._2.value))
      }
    }

    "mapSumToScore" - {
      "on empty set" in {
        val set = SmallIdSet.empty[TopicId]
        val result = set.mapSumToScore(tid => tid.value * 2)
        result should be(Score.Zero)
      }

      "on non-empty set" in {
        val set: SmallIdSet[TopicId] = SmallIdSet(testOkTopicIds *)
        val result = set.mapSumToScore(tid => tid.value * 2)
        result should be(testOkTopicIds.map(_.value * 2).sum)
      }

      "on full set" in {
        val set = SmallIdSet.full[TopicId]
        val result = set.mapSumToScore(tid => tid.value * 2)
        result should be(testAllTopicIds.map(_.value * 2).sum)
      }
    }

    "intersect" - {
      "nominal" in {
        val a = SmallIdSet[TopicId](okIds :+ 3)
        val b = SmallIdSet[TopicId](addedIds :+ 3)
        println(a.toPrettyString)
        println(b.toPrettyString)
        a && b should be(SmallIdSet(3))
      }

      "left is empty" in {
        val a = SmallIdSet[TopicId](okIds *)
        SmallIdSet.empty[TopicId] && a should be(SmallIdSet.empty[SlotId])
      }

      "right is empty" in {
        val a = SmallIdSet[TopicId](okIds *)
        a && SmallIdSet.empty[TopicId] should be(SmallIdSet.empty[SlotId])
      }

      "left is full" in {
        val a = SmallIdSet[TopicId](okIds *)
        SmallIdSet.full[TopicId] && a should be(a)
      }

      "right is full" in {
        val a = SmallIdSet[TopicId](okIds *)
        a && SmallIdSet.full[TopicId] should be(a)
      }
    }

    "filter" - {
      "nominal" in {
        val a = SmallIdSet[TopicId](okIds)
        a.filter(_.value % 3 == 0) should be(SmallIdSet(okIds.filter(_.value % 3 == 0)))
      }

      "set is empty" in {
        val a = SmallIdSet.empty[TopicId]
        a.filter(_.value % 3 == 0) should be(SmallIdSet.empty[TopicId])
      }

      "set is full" in {
        SmallIdSet.full[TopicId].filter(_.value % 3 == 0) should be(SmallIdSet(allIds.filter(_.value % 3 == 0)))
      }

      "filter is always false" in {
        SmallIdSet.empty[TopicId].filter(_ => false) should be(SmallIdSet.empty[TopicId])
        SmallIdSet[TopicId](okIds).filter(_ => false) should be(SmallIdSet.empty[TopicId])
        SmallIdSet.full[TopicId].filter(_ => false) should be(SmallIdSet.empty[TopicId])
      }

      "filter is always true" in {
        SmallIdSet.empty[TopicId].filter(_ => true) should be(SmallIdSet.empty[TopicId])
        SmallIdSet[TopicId](okIds).filter(_ => true) should be(SmallIdSet[TopicId](okIds))
        SmallIdSet.full[TopicId].filter(_ => true) should be(SmallIdSet.full[TopicId])
      }
    }

    "exists" - {
      "nominal" in {
        val a = SmallIdSet[TopicId](okIds)
        a.exists(_.value % 3 == 0) should be(true)
        a.exists(_.value % 3 == 14) should be(false)
        a.exists(_ => true) should be(true)
        a.exists(_ => false) should be(false)
      }

      "set is empty" in {
        val a = SmallIdSet.empty[TopicId]
        a.exists(_.value % 3 == 0) should be(false)
        a.exists(_ => true) should be(false)
      }

      "set is full" in {
        val a = SmallIdSet.full[TopicId]
        a.exists(_.value % 3 == 0) should be(true)
        a.exists(_ => false) should be(false)
      }
    }

    "forall" - {
      "nominal" in {
        val a = SmallIdSet[TopicId](okIds)
        a.forall(_.value >= 0) should be(true)
        a.forall(_.value < 1) should be(false)
        a.forall(_ => true) should be(true)
        a.forall(_ => false) should be(false)
      }

      "set is empty" in {
        val a = SmallIdSet.empty[TopicId]
        a.forall(_.value % 3 == 0) should be(true)
        a.forall(_ => false) should be(true)
      }

      "set is full" in {
        SmallIdSet.full[TopicId].forall(_.value < countAll.value) should be(true)
      }
    }

    "pickRandom" - {

      "always returns the same value if using the same random" in {
        val a = SmallIdSet[SlotId](3, 8, 14, 31, 33, 42, 47, 63)
        val firstPick = a.pickRandom(using Random(0))
        fastLoop(0, 100) { _ =>
          a.pickRandom(using Random(0)) should be(firstPick)
        }
      }

      "only returns values that exist in the set" in {
        given Random = Random(0)

        val a = SmallIdSet[SlotId](3, 8, 47, 63)
        fastLoop(0, 1000) { _ =>
          a.contains(a.pickRandom) should be(true)
        }
      }

      "only returns valid values on a full set" in {
        given Random = Random(0)

        given CountAll[SlotId] = CountAll[SlotId](32)

        val a = SmallIdSet.full[SlotId]
        fastLoop(0, 1000) { _ =>
          a.pickRandom.value should be < 32
        }

      }

      "returns Id.None if the set is empty" in {
        given Random = Random(0)

        val a = SmallIdSet.empty[SlotId]
        a.pickRandom should be(SlotId.None)
      }

    }

    "mergeIfIntersect" - {

      given CountAll[TopicId] = CountAll[TopicId](64)

      "Empty case" in {
        SmallIdSet.mergeIfIntersect(List[SmallIdSet[TopicId]]()) should be(Nil)
      }

      "Just one entry" in {
        SmallIdSet.mergeIfIntersect(List(SmallIdSet[TopicId](1, 2))) should be(List(SmallIdSet[TopicId](1, 2)))
      }

      "Nominal case" in {
        SmallIdSet.mergeIfIntersect(
          SmallIdSet[TopicId](1, 2) :: SmallIdSet[TopicId](3, 4) :: SmallIdSet[TopicId](2, 9) :: SmallIdSet[TopicId](3, 9) :: Nil
        ) should be(List(SmallIdSet[TopicId](1, 2, 3, 4, 9)))
      }

    }

    "Printable" in {
      val a = SmallIdSet[SlotId](3, 8, 47, 63)
      a.toPrettyString should be("[ 3, 8, 47, 63 ]")
    }
  }
}
