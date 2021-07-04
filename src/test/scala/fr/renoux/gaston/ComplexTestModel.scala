package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.input.InputTranscription
import fr.renoux.gaston.model.Score.ScoreIsFractional._
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.util.BitMap.syntax._
import fr.renoux.gaston.util.BitSet.syntax._
import fr.renoux.gaston.util.RandomImplicits._

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.Random

// scalastyle:off magic.number

class ComplexTestModel(seed: Long) {

  private val log = Logger(classOf[ComplexTestModel])

  private val strongPreference = Score(5)
  private val weakPreference = Score(1)

  implicit val random: Random = new Random(seed)

  object Persons {
    private val index = new AtomicInteger(0)
    val All: Array[Person] = Array("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione",
      "isidore", "jennifer", "kevin", "laura", "mike", "natacha", "oliver", "priscilla", "quentin", "rui", "scott",
      "tia", "umar", "valencia", "xavier", "yu", "zelda").map(Person(index.getAndIncrement(), _))
    val AllSet: Set[Person] = All.toSet
  }

  object Topics {
    private val index = new AtomicInteger(0)
    private val topicNames = Array("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota",
      "kappa", "lambda", "mu", "nu", "ksi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi", "khi", "psi",
      "omega")
    val Concrete: Array[Topic] = topicNames.map { n =>
      val mand = random.pick(Persons.All)
      val forb = random.pick(Persons.AllSet - mand)
      Topic(index.getAndIncrement(), n, mandatory = Set(mand), forbidden = Set(forb), min = 4, max = 12)
    }
    val Unassigned: Map[Slot, Topic] = Slots.AllSet.map { s => s -> InputTranscription.unassignedTopic(index.getAndIncrement(), s) }.toMap
    val All: Array[Topic] = Concrete ++ Unassigned.values.toArray
  }

  object Slots {
    private val index = new AtomicInteger(0)
    private val slotNames = Array(Array("d1 am", "d1 pm"), Array("d2 am", "d2 pm"), Array("d3 am", "d3 pm"))
    val AllSequence: Array[Array[Slot]] = slotNames.map(_.map(Slot(index.getAndIncrement(), _, Persons.AllSet -- random.pick(Persons.AllSet, 2))))
    val AllSet: Set[Slot] = AllSequence.flatten.toSet
    val Count = AllSet.size
  }

  object ProblemCounts {
    implicit val CompleteCounts: model.Counts =
      model.Counts(slots = Slots.Count, topics = Topics.All.size, persons = Persons.All.size)
  }

  object Preferences {

    import ProblemCounts.CompleteCounts.implicits._

    val PersonTopics: Array[Preference] = for {
      p <- Persons.All
      (t, i) <- random.pick(Topics.Concrete, 9).zipWithIndex
      str = if (i < 3) strongPreference else weakPreference
    } yield PersonTopicPreference(p, t, str)
    val Incompatibilities: Set[Preference] = Set {
      val p = random.pick(Persons.All, 3)
      PersonGroupAntiPreference(p.head, p.tail.toList.toBitSet, -strongPreference)
    }

    val Unassigned: Set[Preference] = for {
      t: Topic <- Topics.Unassigned.values.toSet
      p <- Persons.All
    } yield PersonTopicPreference(p, t, Score.PersonTotalScore.negative)

    val All: Array[Preference] = PersonTopics ++ Incompatibilities ++ Unassigned
  }

  object Problems {
    val Complete: Problem = {
      import ProblemCounts.CompleteCounts.implicits._
      val p = new ProblemImpl(
        Slots.AllSequence,
        Topics.All,
        Topics.Unassigned.toBitMap(),
        Persons.All,
        Array.empty,
        Preferences.All ++ Preferences.Unassigned
      )
      log.info(s"ComplexTestModel($seed)'s problem is: ${p.toFormattedString}")
      p
    }
  }

}


object ComplexTestModel {
  private val cache = mutable.Map[Long, ComplexTestModel]()

  def apply(seed: Long): ComplexTestModel = cache.getOrElseUpdate(seed, new ComplexTestModel(seed))
}

// scalastyle:on magic.number
