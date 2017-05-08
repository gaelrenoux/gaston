package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class EngineSpec extends FlatSpec with Matchers {
  val log = Logger[EngineSpec]

  import fr.renoux.gaston.TestModel._


  "initializeScheduleForConstraints" should "work a valid partial schedule" in {
    implicit val rand = new Random(0L)
    val solution = Engine.initializeScheduleForConstraints(Problems.Complete)
    log.debug(s"Solution: $solution")
    solution.isDefined should be (true)
    Problems.Complete.constraints.forall { c => !c.isApplicableToPartialSolution || c.isRespected(solution.get) } should be(true)
  }

  "generateRandomSolution" should "work a valid random schedule" in {
    implicit val rand = new Random(0L)
    val partialSolution = Engine.initializeScheduleForConstraints(Problems.Complete)
    val solution = Engine.generateRandomSolution(Problems.Complete, partialSolution.get)
    log.debug(s"Solution: $solution")
    Problems.Complete.constraints.forall { c => c.isRespected(solution) } should be(true)
  }

}
