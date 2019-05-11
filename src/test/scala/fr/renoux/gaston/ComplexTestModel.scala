package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Score.ScoreIsFractional._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.model.problem.ProblemImpl
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.RandomImplicits._

import scala.collection.mutable
import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class ComplexTestModel(seed: Long) {

  private val log = Logger(classOf[ComplexTestModel])

  private val strongPreference = Score(5)
  private val weakPreference = Score(1)

  implicit val random: Random = new Random(seed)

  object Persons {
    val All: Set[Person] = Set("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione",
      "isidore", "jennifer", "kevin", "laura", "mike", "natacha", "oliver", "priscilla", "quentin", "rui", "scott",
      "tia", "umar", "valencia", "xavier", "yu", "zelda").map(Person(_))
  }

  object Topics {
    private val topicNames = Set("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota",
      "kappa", "lambda", "mu", "nu", "ksi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi", "khi", "psi",
      "omega")
    val All: Set[Topic] = topicNames.map(Topic.apply)
    val Bases: Set[Topic] = Slots.AllSet.map(s => Topic.unassigned(s))
  }

  object Slots {
    private val slotNames = Seq(Seq("d1 am", "d1 pm"), Seq("d2 am", "d2 pm"), Seq("d3 am", "d3 pm"))
    val AllSequence: Seq[Seq[Slot]] = slotNames.map(_.map(Slot))
    val AllSet: Set[Slot] = AllSequence.flatten.toSet
  }

  object Constraints {

    val Obligations: Set[Constraint] = for (t <- Topics.All) yield PersonTopicObligation(random.pick(Persons.All), t)

    val Interdictions: Set[Constraint] = (for (t <- random.pick(Topics.All, 5); p <- random.pick(Persons.All, 2))
      yield PersonTopicInterdiction(p, t)).toSet

    val Absences: Set[Constraint] = (
      random.pick(Persons.All, 5).map(PersonAbsence(_, Slot("d1 am"))) ++
        random.pick(Persons.All, 5).map(PersonAbsence(_, Slot("d3 pm")))
      ).toSet

    val Numbers: Set[Constraint] = {
      val initial: Set[Constraint] = Topics.All.map(TopicNeedsNumberOfPersons(_, min = 4, max = 12))
      val toRemove = random.pick(initial, 2)
      val toAdd = toRemove.map { case TopicNeedsNumberOfPersons(t, min, _) => TopicNeedsNumberOfPersons(t, min, 4) }
      initial -- toRemove ++ toAdd
    }

    val BasesForced: Set[Constraint] = Slots.AllSet.map { s => TopicForcedSlot(Topic.unassigned(s), Set(s)) }

    val All: Set[Constraint] = Obligations ++ Interdictions ++ Absences ++ Numbers
    val Bases: Set[Constraint] = BasesForced
  }

  object Preferences {
    val PersonTopics: Set[Preference] = for {
      p <- Persons.All
      (t, i) <- random.pick(Topics.All, 9).zipWithIndex
      str = if (i < 3) strongPreference else weakPreference
    } yield PersonTopicPreference(p, t, str)
    val Incompatibilities: Set[Preference] = Set {
      val p = random.pick(Persons.All, 3)
      PersonGroupAntiPreference(p.head, p.tail.toSet, -strongPreference)
    }

    val All: Set[Preference] = PersonTopics ++ Incompatibilities //+ PersonTopicPreference(Person("brigit kevin"),
    // Topic("sigma"), Score(100))

    val Bases: Set[Preference] = for {
      t <- Topics.Bases
      p <- Persons.All
    } yield PersonTopicPreference(p, t, Score.PersonTotalScore.negative)
  }

  object Problems {
    val Complete: Problem = {
      val p = new ProblemImpl(Slots.AllSequence, Topics.All ++ Topics.Bases, Persons.All, Constraints.All ++ Constraints.Bases, Preferences.All ++ Preferences.Bases)
      log.info(s"ComplexTestModel($seed)'s problem is: ${p.toFormattedString}")
      p
    }
  }

}


object ComplexTestModel {
  private val cache = mutable.Map[Long, ComplexTestModel]()

  def apply(seed: Long): ComplexTestModel = cache.getOrElseUpdate(seed, new ComplexTestModel(seed))
}
