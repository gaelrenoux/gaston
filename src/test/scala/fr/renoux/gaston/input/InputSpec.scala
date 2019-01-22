package fr.renoux.gaston.input

import java.io.File

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.PersonTopicPreference
import fr.renoux.gaston.model.problem.Problem
import org.scalatest.{FlatSpec, Matchers}

class InputSpec extends FlatSpec with Matchers {

  "Loading from default" should "load the default input when no name is given" in {
    val (input, _) = InputLoader.fromDefault.forceToInputAndModel
    input.gaston.settings.incompatibilityAntiPreference should be(Score(-50))
    input.gaston.persons.size should be(3)
  }

  "Loading from the classpath" should "load the correct input" in {
    val (input, _) = InputLoader.fromClassPath("named-configuration.conf").forceToInputAndModel
    input.gaston.settings.incompatibilityAntiPreference should be(Score(-42))
    input.gaston.persons.size should be(1)
  }

  "Loading from a file" should "load the correct input" in {
    val stringPath = getClass.getResource("/named-configuration.conf").getPath
    val path = new File(stringPath).toPath
    val (input, _) = InputLoader.fromPath(path).forceToInputAndModel
    input.gaston.settings.incompatibilityAntiPreference should be(Score(-42))
    input.gaston.persons.size should be(1)
  }

  "Checking the sample" should "work" in {
    InputLoader.fromClassPath("sample.conf").forceToInputAndModel
  }


  val minimalProblem: Problem = InputLoader.fromDefault.forceToModel

  "Produced problem" should "contain the correct parallelization" in {
    minimalProblem.parallelization should be(2)
  }

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
      TopicsSimultaneous(Set(Topic("alpha"), Topic("beta"))),
      TopicsExclusive(Set(Topic("beta"), Topic("gamma")), Set(Person("laverne")))
    ))
  }

  it should "contain the correct preferences" in {
    minimalProblem.preferences should be(Set(
      PersonTopicPreference(Person("bernard", Weight(1.0)), Topic("alpha"), Score(5.0)),
      PersonTopicPreference(Person("bernard", Weight(1.0)), Topic("beta"), Score(1.0)),
      PersonTopicPreference(Person("bernard", Weight(1.0)), Topic("gamma"), Score(1.0))
    ))
  }

}