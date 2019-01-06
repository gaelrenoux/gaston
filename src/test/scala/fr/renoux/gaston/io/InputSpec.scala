package fr.renoux.gaston.io

import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints.{PersonTopicInterdiction, PersonTopicObligation, TopicNeedsNumberOfPersons}
import fr.renoux.gaston.model.preferences.PersonTopicPreference
import org.scalatest.{FlatSpec, Matchers}

class InputSpec extends FlatSpec with Matchers {

  behavior of "fromClassPath"
  it should "load the input from the ClassPath" in {
    val (input, problem) = PureConfigLoader.fromClassPath.forceToInputAndModel
    input.gaston.settings.weakPreference should be(Score(1))
    problem.persons.size should be > 0
  }

  "conversion" should "work" in {
    val problem = PureConfigLoader.fromClassPath.forceToModel
    problem.slots should be(UdoConTestModel.Problems.Simplified.slots)
    problem.topics should be(UdoConTestModel.Problems.Simplified.topics)
    problem.persons should be(UdoConTestModel.Problems.Simplified.persons)
    problem.constraints should be(UdoConTestModel.Problems.Simplified.constraints)
    problem.preferences should be(UdoConTestModel.Problems.Simplified.preferences)
    problem should be(UdoConTestModel.Problems.Simplified)
  }

  val minimalProblem = PureConfigLoader.fromClassPath("application-min.conf").forceToModel

  "minimal configuration" should "contain the correct slots" in {
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
      TopicNeedsNumberOfPersons(Topic("beta"), 4, 5)
    ))
  }

  it should "contain the correct preferences" in {
    minimalProblem.preferences should be(Set(
      PersonTopicPreference(Person("bernard", Weight(1.0)), Topic("alpha"), Score(5.0)),
      PersonTopicPreference(Person("bernard", Weight(1.0)), Topic("beta"), Score(1.0))
    ))
  }

}
