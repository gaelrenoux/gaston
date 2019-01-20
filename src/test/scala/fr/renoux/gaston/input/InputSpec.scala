package fr.renoux.gaston.input

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints.{PersonTopicInterdiction, PersonTopicObligation, SimultaneousTopics, TopicNeedsNumberOfPersons}
import fr.renoux.gaston.model.preferences.PersonTopicPreference
import fr.renoux.gaston.model.problem.Problem
import org.scalatest.{FlatSpec, Matchers}

class InputSpec extends FlatSpec with Matchers {

  "Loading from the classpath" should "load the default input when no name is given" in {
    val (input, _) = PureConfigLoader.fromClassPath.forceToInputAndModel
    input.gaston.settings.strongPreference should be(Score(5))
    input.gaston.persons.size should be(3)
  }

  it should "load an input with the correct name if asked" in {
    val (input, _) = PureConfigLoader.fromClassPath("named-configuration.conf").forceToInputAndModel
    input.gaston.settings.strongPreference should be(Score(42))
    input.gaston.persons.size should be(1)
  }


  val minimalProblem: Problem = PureConfigLoader.fromClassPath.forceToModel

  "Produced problem" should "contain the correct slots" in {
    minimalProblem.slots should be(Set(Slot("A"), Slot("B")))
  }

  it should "contain the correct topics" in {
    minimalProblem.topics should be(Set(Topic("alpha"), Topic("beta"), Topic("gamma")))
  }

  it should "contain the correct persons" in {
    minimalProblem.persons should be(Set(Person("bernard", Weight
      .Default), Person("laverne", Weight(1)), Person("hoagie", Weight(1.5))))
  }

  it should "contain the correct constraints" in {
    minimalProblem.constraints should be(Set(
      PersonTopicObligation(Person("bernard", Weight(1.0)), Topic("alpha")),
      PersonTopicInterdiction(Person("laverne"), Topic("beta")),
      TopicNeedsNumberOfPersons(Topic("alpha"), 5, 5),
      TopicNeedsNumberOfPersons(Topic("gamma"), 4, 6),
      TopicNeedsNumberOfPersons(Topic("beta"), 4, 5),
      SimultaneousTopics(Set(Topic("alpha"), Topic("beta")))
    ))
  }

  it should "contain the correct preferences" in {
    minimalProblem.preferences should be(Set(
      PersonTopicPreference(Person("bernard", Weight(1.0)), Topic("alpha"), Score(5.0)),
      PersonTopicPreference(Person("bernard", Weight(1.0)), Topic("beta"), Score(1.0))
    ))
  }

}
