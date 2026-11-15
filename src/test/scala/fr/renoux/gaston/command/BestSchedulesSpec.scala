package fr.renoux.gaston.command

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.engine.{GreedyEngine, Termination}
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class BestSchedulesSpec extends AnyFlatSpec with Matchers {

  private given Context = Context.Default

  private given Problem = SimpleTestModel.Problems.WithUnassignedTopics

  /** Schedules from multiple chains, in a random order */
  private lazy val schedules: Seq[Schedule] = {
    val engine = new GreedyEngine
    val all = (0L until 10L).flatMap { seed => engine.lazySeq(seed, Termination(count = Some(30))).map(_._1) }
    new Random(42).shuffle(all)
  }

  /** Straightforward but slow implementation, to compare with */
  private def expected(capacity: Int, input: Seq[Schedule]): Seq[Schedule] =
    input.groupBy(_.chainSeed).values.map(_.maxBy(_.score.value)).toSeq.sortBy(-_.score.value).take(capacity)

  "BestSchedules" should "be empty at first" in {
    BestSchedules.empty(3).schedules should be(empty)
    BestSchedules.empty(3).best should be(None)
  }

  it should "reject a zero capacity" in {
    an[IllegalArgumentException] should be thrownBy BestSchedules.empty(0)
  }

  it should "keep the best schedules, at most one per chain" in {
    schedules.map(_.chainSeed).distinct.size should be > 5 // sanity check on test data
    val result = schedules.foldLeft(BestSchedules.empty(5))(_.add(_))
    result.schedules.map(_.score.value) should be(expected(5, schedules).map(_.score.value))
    result.schedules.map(_.chainSeed).distinct.size should be(5)
    result.best should be(result.schedules.headOption)
  }

  it should "keep fewer schedules if there are not enough chains" in {
    val fromTwoChains = schedules.filter(s => s.chainSeed == 0L || s.chainSeed == 1L)
    val result = fromTwoChains.foldLeft(BestSchedules.empty(5))(_.add(_))
    result.schedules.map(_.score.value) should be(expected(5, fromTwoChains).map(_.score.value))
    result.schedules.size should be(2)
  }

  it should "merge correctly" in {
    val (left, right) = schedules.splitAt(schedules.size / 2)
    val merged = left.foldLeft(BestSchedules.empty(5))(_.add(_)).merge(right.foldLeft(BestSchedules.empty(5))(_.add(_)))
    merged.schedules.map(_.score.value) should be(expected(5, schedules).map(_.score.value))
  }
}
