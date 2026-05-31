package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.util.Count as _


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

    "Creation" - {
      "empty set" in {
        val set = SmallIdSet.empty[TopicId]
        set.underlying should be(0)
        set.isEmpty should be(true)
        set.toSet.isEmpty should be(true)
      }
    }

 }
}
