package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.input.Constants
import fr.renoux.gaston.model.*
import fr.renoux.gaston.model.Score.given
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.util.RandomImplicits.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.math.Fractional.Implicits.infixFractionalOps
import scala.util.Random

//TODO Add a version with unassigned topics
class ComplexTestModel(seed: Long) {

  private val log = Logger(classOf[ComplexTestModel])

  private val strongPreference = Score(5)
  private val weakPreference = Score(1)

  implicit val random: Random = new Random(seed)

  object Persons {
    private val index = new AtomicInteger(0)
    val All: Set[Person] = Set("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione",
      "isidore", "jennifer", "kevin", "laura", "mike", "natacha", "oliver", "priscilla", "quentin", "rui", "scott",
      "tia", "umar", "valencia", "xavier", "yu", "zelda").map(Person(index.getAndIncrement(), _))
  }

  object Topics {
    private val index = new AtomicInteger(0)
    private val topicNames = Set("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota",
      "kappa", "lambda", "mu", "nu", "ksi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi", "khi", "psi",
      "omega")
    val Concrete: Set[Topic] = topicNames.map { n =>
      val mand = random.pick(Persons.All)
      val forb = random.pick(Persons.All - mand)
      Topic(index.getAndIncrement(), n, mandatory = Set(mand), forbidden = Set(forb), min = 4, max = 12)
    }
    val Unassigned: Map[Slot, Topic] = Slots.AllSet.map { s => s -> Topic.unassigned(index.getAndIncrement(), s) }.toMap
    val All: Set[Topic] = Concrete ++ Unassigned.values
  }

  object Slots {
    private val index = new AtomicInteger(0)
    private val slotNames = Seq(Seq("d1 am", "d1 pm"), Seq("d2 am", "d2 pm"), Seq("d3 am", "d3 pm"))
    val AllSequence: Seq[Seq[Slot]] = slotNames.map { sequence =>
      var nextSlot: Option[Slot] = None
      sequence.reverseIterator.map { name =>
        val slot = Slot(index.getAndIncrement(), name, Persons.All -- random.pick(Persons.All, 2), nextSlot)
        nextSlot = Some(slot)
        slot
      }.toSeq
    }
    val AllSet: Set[Slot] = AllSequence.flatten.toSet
    val Count = AllSet.size
  }

  object ProblemCounts {
    implicit val CompleteCounts: model.Counts =
      model.Counts(slots = Slots.Count, topics = Topics.All.size, persons = Persons.All.size)
  }

  object Preferences {

    import ProblemCounts.CompleteCounts

    val PersonTopics: Set[Preference] = for {
      p <- Persons.All
      (t, i) <- random.pick(Topics.Concrete, 9).zipWithIndex
      str = if (i < 3) strongPreference else weakPreference
    } yield PersonTopicPreference(p, t, str)
    val Incompatibilities: Set[Preference] = Set {
      val p = random.pick(Persons.All, 3)
      PersonGroupAntiPreference(p.head, p.tail.toArraySet, -strongPreference)
    }

    val Unassigned: Set[Preference] = for {
      t: Topic <- Topics.Unassigned.values.toSet
      p <- Persons.All
    } yield PersonTopicPreference(p, t, Constants.PersonTotalScore.negative)

    val All: Set[Preference] = PersonTopics ++ Incompatibilities ++ Unassigned
  }

  object Problems {
    val Complete: Problem = {
      import ProblemCounts.CompleteCounts
      val p = new Problem(
        Slots.AllSequence,
        Topics.All,
        Topics.Unassigned.toArrayMap,
        Persons.All,
        Set.empty,
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
