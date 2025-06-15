package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.force
import fr.renoux.gaston.input.{InputLoader, InputModel, InputTranscription2}


class SmallProblemTest extends TestBase {

  val input: InputModel = InputLoader.fromClassPath("transcription-test.conf").force
  val transcription = InputTranscription2(input)
  val problem: SmallProblem = transcription.problem

  import problem.given

  val Seq(albert: PersonId, bianca: PersonId, charly: PersonId) = problem.personsCount.range.toSeq
  val Seq(
  unassignedD1a, unassignedD1e, unassignedD2a, unassignedD3a, unassignedD3e, unassignedD3n, // 0 to 5
  alpha, beta, gamma, delta, // 6 to 9
  epsilon1, epsilon2, eta1, eta2, // 10 to 13
  theta11, theta12, theta21, theta22, theta31, theta32 // 14 to 19
  ) = problem.topicsCount.range

  "topicsToLinkedTopics" in {
    val expected = IdMap(
      alpha -> SmallIdSet(eta1, eta2),
      eta1 -> SmallIdSet(alpha, eta2),
      eta2 -> SmallIdSet(alpha, eta1),
      theta11 -> SmallIdSet(theta12),
      theta12 -> SmallIdSet(theta11),
      theta21 -> SmallIdSet(theta22),
      theta22 -> SmallIdSet(theta21),
      theta31 -> SmallIdSet(theta32),
      theta32 -> SmallIdSet(theta31)
    )
    problem.topicsToLinkedTopics(alpha).toSet should be(Set(eta1, eta2))
    problem.topicsToLinkedTopics should be(expected)
  }

  "personTopicsMandatory" in {
    val expected = IdMap(
      albert -> SmallIdSet(alpha, delta),
      bianca -> SmallIdSet(alpha),
      charly -> SmallIdSet(theta11, theta12, theta21, theta22, theta31, theta32),
    )
    problem.personsToMandatoryTopics should be(expected)
  }

  "personsWithPersonWish" in {
    val expected = SmallIdSet(albert, charly)
    problem.personsWithPersonWish should be(expected)
  }

  "personsTargetedToPersonsWithWish" in {
    val expected = IdMap(
      albert -> SmallIdSet(charly),
      bianca -> SmallIdSet(albert),
      charly -> SmallIdSet.empty
    )
    problem.personsTargetedToPersonsWithWish should be(expected)
  }

  "isTopicUnassigned" in {
    problem.isTopicUnassigned(unassignedD1a) should be(true)
    problem.isTopicUnassigned(unassignedD3n) should be(true)
    problem.isTopicUnassigned(alpha) should be(false)
    problem.isTopicUnassigned(epsilon1) should be(false)
    problem.isTopicUnassigned(theta32) should be(false)
  }

  "isPersonMandatory" in {
    problem.isPersonMandatory(albert, alpha) should be(true)
    problem.isPersonMandatory(albert, beta) should be(false)
    problem.isPersonMandatory(bianca, delta) should be(false)
    problem.isPersonMandatory(charly, eta2) should be(false)
    problem.isPersonMandatory(charly, theta11) should be(true)
    problem.isPersonMandatory(charly, theta22) should be(true)
  }

  "isPersonForbidden" in {
    problem.isPersonForbidden(albert, alpha) should be(false)
    problem.isPersonForbidden(albert, eta2) should be(false)
    problem.isPersonForbidden(bianca, epsilon1) should be(true)
    problem.isPersonForbidden(bianca, epsilon2) should be(true)
    problem.isPersonForbidden(bianca, eta1) should be(false)
    problem.isPersonForbidden(bianca, eta2) should be(false)
    problem.isPersonForbidden(charly, epsilon1) should be(false)
    problem.isPersonForbidden(charly, epsilon2) should be(false)
    problem.isPersonForbidden(charly, eta1) should be(true)
    problem.isPersonForbidden(charly, eta2) should be(true)
  }

  "getTopicIdByName" in {
    problem.getTopicIdByName("Delta") should be(delta)
    problem.getTopicIdByName("Epsilon #2") should be(epsilon2)
    problem.getTopicIdByName("Eta ~2") should be(eta2)
    problem.getTopicIdByName("Theta #3 ~2") should be(theta32)
  }


  "copy" in {
    val problem2 = problem.copy()

    classOf[SmallProblem].getFields.foreach { field =>
      field.get(problem2) should be(field.get(problem))
    }

    val uncheckedMethods = Set(
      "copy", // that's what we're checking, and also SmallProblem don't have a usable equals method
      "hashCode", "equals", // Keeping the native implementations, so different objects will have different values
      "getClass", "notify", "notifyAll", "wait" // technical methods
    )
    classOf[SmallProblem].getMethods.filter(_.getParameterCount == 0)
      .filterNot { m => uncheckedMethods.contains(m.getName) }
      .foreach { method =>
        method.invoke(problem2) should be(method.invoke(problem))
      }
  }


}
