package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.InputSettings
import fr.renoux.gaston.model.Score.ScoreIsFractional._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, Preference}
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Slot, Topic}
import fr.renoux.gaston.util.RandomImplicits._

import scala.collection.mutable
import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class ComplexTestModel(seed: Long)(implicit settings: InputSettings) {

  private val log = Logger(classOf[ComplexTestModel])

  implicit val random = new Random(seed)

  object Persons {
    val All: Set[Person] = Set("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione", "isidore", "jennifer", "kevin", "laura", "mike", "natacha", "oliver", "priscilla", "quentin", "rui", "scott", "tia", "umar", "valencia", "xavier", "yu", "zelda").map(Person(_))
  }

  object Topics {
    private val topicNames = Set("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu", "nu", "ksi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi", "khi", "psi", "omega")
    val All: Set[Topic] = topicNames map Topic
  }

  object Slots {
    private val slotNames = Set("d1 am", "d1 pm", "d2 am", "d2 pm", "d3 am", "d3 pm")
    val All: Set[Slot] = slotNames map Slot
  }

  object Constraints {

    val Obligations: Set[Constraint] = for (t <- Topics.All) yield PersonTopicObligation(random.pick(Persons.All), t)

    val Interdictions: Set[Constraint] = (for (t <- random.pick(Topics.All, 5); p <- random.pick(Persons.All, 2)) yield PersonTopicInterdiction(p, t)).toSet

    val Absences: Set[Constraint] = (
      (random.pick(Persons.All, 5) map (PersonAbsence(_, Slot("d1 am")))) ++
        (random.pick(Persons.All, 5) map (PersonAbsence(_, Slot("d3 pm"))))
      ).toSet

    val Numbers: Set[Constraint] = {
      val initial: Set[Constraint] = Topics.All map (TopicNeedsNumberOfPersons(_, min = 4, max = 12))
      val toRemove = random.pick(initial, 2)
      val toAdd = toRemove map { case TopicNeedsNumberOfPersons(t, min, _) => TopicNeedsNumberOfPersons(t, min, 4) }
      initial -- toRemove ++ toAdd
    }

    val All: Set[Constraint] = Obligations ++ Interdictions ++ Absences ++ Numbers
  }

  object Preferences {
    val PersonTopics: Set[Preference] = for {
      p <- Persons.All
      (t, i) <- random.pick(Topics.All, 9).zipWithIndex
      str = if (i < 3) settings.strongPreference else settings.weakPreference
    } yield PersonTopicPreference(p, t, str)
    val Incompatibilities: Set[Preference] = Set {
      val p = random.pick(Persons.All, 3)
      PersonGroupAntiPreference(p.head, p.tail.toSet, -settings.strongPreference)
    }

    val All: Set[Preference] = PersonTopics ++ Incompatibilities //+ PersonTopicPreference(Person("brigit kevin"), Topic("sigma"), Score(100))
  }

  object Problems {
    val Complete = {
      val p = Problem(Slots.All, Topics.All, Persons.All, Constraints.All, Preferences.All)
      log.info(s"ComplexTestModel($seed)'s problem is: ${p.toFormattedString}")
      p
    }
  }

}


object ComplexTestModel {
  private val cache = mutable.Map[Long, ComplexTestModel]()

  def apply(seed: Long)(implicit settings: InputSettings): ComplexTestModel = cache.getOrElseUpdate(seed, new ComplexTestModel(seed))
}
