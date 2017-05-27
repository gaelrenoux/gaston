package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class ConstrainedScheduleFactorySpec extends FlatSpec with Matchers {
  val log = Logger[ConstrainedScheduleFactorySpec]

  import fr.renoux.gaston.TestModel._

  "makePartialSchedule" should "work a partial schedule when there is one" in {
    val factory = new ConstrainedScheduleFactory(Problems.Complete)
    val partialSolution = factory.makePartialSchedule(new Random(0L))
    log.debug(s"Solution: $partialSolution")
    Problems.Complete.constraints.forall { c => !c.isApplicableToPartialSolution || c.isRespected(Solutions.Perfect) } should be(true)
    partialSolution.isDefined should be (true)
  }

  it should "always return a valid partial schedule if any" in {
    val factory = new ConstrainedScheduleFactory(Problems.Complete)
    val partialSolution = factory.makePartialSchedule(new Random(0L))
    log.debug(s"Solution: $partialSolution")
    partialSolution foreach { s =>
      Problems.Complete.constraints.forall { c => !c.isApplicableToPartialSolution || c.isRespected(s) } should be(true)
    }
  }

}
