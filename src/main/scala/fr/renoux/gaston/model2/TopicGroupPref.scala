package fr.renoux.gaston.model2

/** Designates a group of topics for which we add some score when they are present multiple times in an evaluated set.
  *
  * Typically, this is used to evaluate all topics for a single person and to check exclusive topics (so the score is
  * negative), but it can be used for any case where we went to apply a preference on having multiple topics.
  */
final case class TopicGroupPref(
    topics: SmallIdSet[TopicId],
    score: Score
) {

  def evaluate(tids: SmallIdSet[TopicId]): Score = {
    val matchingTidsCount = (tids && topics).size
    if (matchingTidsCount > 1) {
      score * (matchingTidsCount.value - 1)
    } else Score.Zero
  }

  override def toString: String = s"""TopicGroupPref(${topics.toPrettyString}, $score)"""
}
