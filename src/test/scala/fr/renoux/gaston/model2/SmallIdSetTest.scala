package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase


class SmallIdSetTest extends TestBase {

  /* OK ids and added id must be distinct */
  /* Make sure there's always at least one multiple of 3 and one non-multilpe of 3 */
  "With a max count" - {
    behave like behaviorFor(List(3, 8, 24, 47, 61), List(12, 19, 41))(using Count(64))
  }
  "With a lower count" - {
    behave like behaviorFor(List(4, 5, 9), List(3, 6, 7))(using Count(12))
  }
  "Having both min and max value" - {
    behave like behaviorFor(List(0, 34, 63), List(1, 15, 62))(using Count(64))
  }

  def behaviorFor(okIds: Seq[Id], addedIds: Seq[Id])(using countAll: Count[Id]): Unit = {

    "Creation" - {
      "empty set" in {
        val set = SmallIdSet.empty[Id]
        set.isEmpty should be(true)
      }
    }

 }
}
