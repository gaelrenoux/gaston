package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class PreferredScheduleFactorySpec extends FlatSpec with Matchers {
  val log = Logger[PreferredScheduleFactorySpec]

  import fr.renoux.gaston.TestModel._

  "generateRandomSolution" should "work a valid random schedule" in {
    implicit val random = new Random(0L)

    val csFactory = new ConstrainedScheduleFactory(Problems.Complete)
    val partialSolution = csFactory.makePartialSchedule

    val psFactory = new PreferredScheduleFactory(Problems.Complete)
    val solution = psFactory.completePartialSchedule(partialSolution.get)

    log.debug(s"Solution: $solution")
    Problems.Complete.constraints.forall { c => c.isRespected(solution) } should be(true)
  }

}
