package fr.renoux.gaston.model

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class SolutionSpec extends FlatSpec with Matchers {
  val log = Logger[SolutionSpec]

  import TestModel._

  "Solution score" should "be the sum of constraint scores" in {
    val solutionScore = Solutions.Perfect.score(Constraints.All)
    log.debug(s"Solution score: $solutionScore")
    val constraintScores = Constraints.All.toSeq map (_.evaluate(Solutions.Perfect))
    log.debug(s"Constraints scores: $constraintScores")
    solutionScore should be(constraintScores.sum)
  }

  it should "be minus infinite for one or more missed mandatory constraints" in {
    val solutionScore = Solutions.Terrible.score(Constraints.All)
    solutionScore should be(Double.NegativeInfinity)
  }

}
