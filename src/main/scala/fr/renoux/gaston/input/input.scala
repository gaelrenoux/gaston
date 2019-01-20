package fr.renoux.gaston.input

import fr.renoux.gaston.model._

/* All line and column indices are zero-based */

case class InputRoot(
    gaston: InputModel
)

case class InputModel(
    settings: InputSettings,
    udoSettings: Option[InputUdoSettings],
    slots: Set[String],
    persons: Set[InputPerson],
    topics: Set[InputTopic],
    constraints: Option[InputGlobalConstraints] = None
)

case class InputSettings(
    parallelization: Option[Int] = None,
    incompatibilityAntiPreference: Score,
    defaultMin: Int,
    defaultMax: Int
)

case class InputUdoSettings(
    /* Column at which the persons start on the first line */
    personsStartingIndex: Int,
    /* Column for the topics */
    topicsIndex: Int,
    /* Column for the min number of persons on that topic */
    minPlayersIndex: Option[Int],
    /* Column for the max number of persons on that topic */
    maxPlayersIndex: Int,
    /* Weight given to any gamemaster */
    gamemasterWeight: Weight,
    /* Score given to a strong wish */
    strongWishValue: Score,
    /* Score given to a weak wish */
    weakWishValue: Score
)

case class InputPerson(
    name: String,
    weight: Weight = Weight.Default,
    absences: Set[String] = Set(),
    mandatory: Set[String] = Set(),
    forbidden: Set[String] = Set(),
    incompatible: Set[String] = Set(),
    wishes: Set[InputPersonWishes] = Set()
)

case class InputPersonWishes(
    value: Score,
    topics: Set[String] = Set()
)

case class InputTopic(
    name: String,
    min: Option[Int],
    max: Option[Int],
    forcedSlot: Option[String] = None
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