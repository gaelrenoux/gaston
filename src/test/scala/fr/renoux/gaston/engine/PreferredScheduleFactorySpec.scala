package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class PreferredScheduleFactorySpec extends FlatSpec with Matchers {
  val log = Logger[PreferredScheduleFactorySpec]

  import fr.renoux.gaston.ComplexTestModel._

  "generateRandomSolution" should "work a valid random schedule" in {
    for (seed <- 0L until 10L) {
      implicit val random: Random = new Random(seed)
      log.debug(s"Seed: $seed")

      val csFactory = new ConstrainedScheduleFactory(Problems.Complete)
      val partialSolution = csFactory.makePartialSchedule
      log.debug(s"Partial solution: $partialSolution")

      val psFactory = new PreferredScheduleFactory(Problems.Complete)
      val tempSolution = psFactory.completePartialSchedule(partialSolution.get)
      log.debug(s"Temporary solution: $tempSolution")
      val finalSolution = psFactory.improveUntilItChecks(tempSolution)
      log.debug(s"Solution: $finalSolution")

      Problems.Complete.isSolvedBy(finalSolution) should be(true)
    }
  }

}
