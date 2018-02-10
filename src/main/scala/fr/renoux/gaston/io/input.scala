package fr.renoux.gaston.io

import fr.renoux.gaston.model._

/* TODO try without the Options, using default values */

case class InputRoot(
                      gaston: InputModel
                    )

case class InputModel(
                       settings: InputSettings,
                       slots: Set[String],
                       persons: Set[InputPerson],
                       topics: Set[InputTopic],
                       preferences: Option[Set[InputPreference]]
                     )

case class InputSettings(
                          weakPreference: Score,
                          strongPreference: Score,
                          incompatibilityAntiPreference: Score
                        )

case class InputPreference(
                            person: String,
                            strong: Option[Set[String]],
                            weak: Option[Set[String]]
                          )

case class InputPerson(
                        name: String,
                        weight: Option[Double],
                        incompatible: Option[Set[String]],
                        absences: Option[Set[String]]
                      )

case class InputTopic(
                       name: String,
                       mandatory: Option[Set[String]],
                       forbidden: Option[Set[String]],
                       min: Option[Int],
                       max: Option[Int],
                       forcedSlot: Option[String]
                     )