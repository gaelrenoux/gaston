package fr.renoux.gaston.input

import fr.renoux.gaston.model._

/* All line and column indices are zero-based */

case class InputRoot(
    gaston: InputModel
)

case class InputModel(
    settings: InputSettings = InputSettings(),
    tableSettings: Option[InputTableSettings] = None,
    slots: Seq[Seq[InputSlot]] = Seq(),
    persons: Set[InputPerson] = Set(),
    topics: Set[InputTopic] = Set(),
    constraints: InputGlobalConstraints = InputGlobalConstraints()
)

case class InputSettings(
    incompatibilityAntiPreference: Score = Score(-1000),
    defaultMaxTopicsPerSlot: Option[Int] = None,
    defaultMinPersonsPerTopic: Int = 1,
    defaultMaxPersonsPerTopic: Int = 10,
    maxPersonsOnNothing: Int = 0,
    minPersonsOnNothing: Int = 0,
    personOnNothingAntiPreference: Score = Score.Zero,
    backtrackInitialSchedule: Boolean = true //TODO Should be calculated
)

case class InputTableSettings(
    separator: String = "\t",
    personsRow: Int = 0,
    wishesStartRow: Int = 1,
    personsStartIndex: Int = 4,
    topicIndex: Int = 0,
    topicOccurrenceCountIndex: Option[Int] = None,
    mandatoryPersonIndex: Int = 1,
    minPersonsIndex: Option[Int] = None,
    maxPersonsIndex: Int = 3,
    personsCountAdd: Int = 0,
    mandatoryPersonWeight: Weight = Weight.Default,
    forbiddenPersonMarker: Option[String] = None,
    preferencesScoreMapping: Option[Map[String, Score]] = None
)

case class InputSlot(
    name: String,
    maxTopics: Option[Int] = None
)

case class InputTopic(
    name: String,
    min: Option[Int] = None,
    max: Option[Int] = None,
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