package fr.renoux.gaston.input

import fr.renoux.gaston.model._

/* All line and column indices are zero-based */

case class InputRoot(
    gaston: InputModel
)

case class InputModel(
    settings: InputSettings,
    tableSettings: Option[InputTableSettings],
    slots: Seq[Seq[InputSlot]],
    persons: Set[InputPerson],
    topics: Set[InputTopic],
    constraints: Option[InputGlobalConstraints] = None
)

case class InputSettings(
    incompatibilityAntiPreference: Score,
    defaultMaxTopicsPerSlot: Option[Int] = None,
    defaultMinPersonsPerTopic: Int,
    defaultMaxPersonsPerTopic: Int,
    /* Maximum number of persons that can have no topic on a slot */
    maxPersonsOnNothing: Int = 0,
    /* Minimum number of persons that can have no topic on a slot (so that they can do something else) */
    minPersonsOnNothing: Int = 0,
    /* Negative score for someone not doing anything */
    personOnNothingAntiPreference: Score = Score.Zero,
    /* Should initial schedules on each iteration be empty or backtracked ? */
    backtrackInitialSchedule: Boolean = true //TODO Should be calculated
)

case class InputTableSettings(
    separator: String,
    /* Row where the persons' names appear */
    personsRow: Int,
    /* Additional header rows to ignore */
    otherHeaderRowsCount: Int,
    /* Column at which the persons start */
    personsStartingIndex: Int,
    /* Column for the topics */
    topicIndex: Int,
    /* Column for the min number of persons on that topic */
    minPersonsIndex: Option[Int],
    /* Column for the max number of persons on that topic */
    maxPersonsIndex: Int,
    /* Number to add to the count of persons (typically because min and max don't always include the mandatory person) */
    personsCountAdd: Int,
    /* Index for one mandatory's person' name */
    mandatoryPersonIndex: Int,
    /* Index for the count of occurrences of a topic */
    topicOccurrenceCountIndex: Option[Int],
    /* If a person is mandatory on one topic, his preferences will weight more on other topics (as a reward). Should be
     higher than one. */
    mandatoryPersonRewardWeight: Weight,
    /* What text marks the person as forbidden on that topic */
    forbiddenPersonMarker: Option[String],
    /* For each preference value, how it should be translated to a score (unknown texts are ignored). If no mapping is
    provided, the raw numerical values will be used.  */
    preferencesScoreMapping: Option[Map[String, Score]]
)

case class InputSlot(
    name: String,
    maxTopics: Option[Int] = None
)

case class InputTopic(
    name: String,
    min: Option[Int],
    max: Option[Int],
    occurrences: Option[Int] = None,
    slots: Option[Set[String]] = None
) {
  /** Occurrence needs to be an Option to not appear when not needed */
  lazy val forcedOccurrences: Int = occurrences.getOrElse(1)
}

case class InputPerson(
    name: String,
    weight: Weight = Weight.Default,
    absences: Set[String] = Set(),
    mandatory: Set[String] = Set(),
    forbidden: Set[String] = Set(),
    incompatible: Set[String] = Set(),
    wishes: Map[String, Score] = Map()
)

case class InputGlobalConstraints(
    simultaneous: Set[InputSimultaneousConstraint] = Set(),
    exclusive: Set[InputExclusiveConstraint] = Set()
)

case class InputSimultaneousConstraint(
    topics: Set[String]
)

case class InputExclusiveConstraint(
    topics: Set[String],
    exemptions: Set[String] = Set()
)