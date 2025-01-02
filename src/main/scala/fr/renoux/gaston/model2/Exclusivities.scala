package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

final class Exclusivities(
    val groups: Array[SmallIdSet[TopicId]],
    val scores: Array[Score]
) {
  assert(groups.length == scores.length)

  lazy val count: Int = groups.length

  def evaluate(tids: SmallIdSet[TopicId]): Score = {
    var result = Score.Zero
    fastLoop(0, count) { i =>
      val matchingTidsCount = (tids && groups(i)).size
      if (matchingTidsCount > 1) {
        result += scores(i) * (matchingTidsCount.value - 1)
      }
    }
    result
  }
}
