package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.input.InputModel
import fr.renoux.gaston.input.InputLoader
import fr.renoux.gaston.input.InputTranscription2
import fr.renoux.gaston.TestUtils.force


class SmallProblemTest extends TestBase {

  // TODO test copy
  // TODO test basic methods

  val input: InputModel = InputLoader.fromClassPath("transcription-test.conf").force
  val transcription = InputTranscription2(input)
  val problem = transcription.problem

  "personsWithPersonWish" in {
    problem.personsWithPersonWish should be(SmallIdSet(0, 2))
  }

}
