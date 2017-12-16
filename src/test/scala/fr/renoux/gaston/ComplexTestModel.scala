package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.Definitions
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, Preference}
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Slot, Topic}
import fr.renoux.gaston.util.RandomImplicits._

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
object ComplexTestModel {

  private val log = Logger(ComplexTestModel.getClass)

  implicit val random = new Random(25756L)

  object Persons {
    private val firstNames = Set("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione", "isidore", "jennifer", "kevin", "laura", "mike", "natacha", "oliver", "priscilla", "quentin", "rui", "scott", "tia", "umar", "valencia", "xavier", "yu", "zelda")
    private val lastNames = Set("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione", "isidore", "jennifer", "kevin", "laura", "mike", "natacha", "oliver", "priscilla", "quentin", "rui", "scott", "tia", "umar", "valencia", "xavier", "yu", "zelda")
    private val personsSeq = for (i <- 0 until 30) yield Person(s"${random.pick(firstNames)} ${random.pick(lastNames)}")
    val All: Set[Person] = personsSeq.toSet
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
      val initial = Topics.All map (TopicNeedsNumberOfPersons(_, min = 4, max = 12))
      val toRemove = random.pick(initial, 2)
      val toAdd = toRemove map { case TopicNeedsNumberOfPersons(t, min, _) => TopicNeedsNumberOfPersons(t, min, 4) }
      initial -- toRemove ++ toAdd
    }

    val All: Set[Constraint] = Obligations ++ Interdictions ++ Absences ++ Numbers
  }

  object Preferences {
    val All: Set[Preference] = for {
      p <- Persons.All
      (t, i) <- random.pick(Topics.All, 9).zipWithIndex
      str = if (i < 3) Definitions.StrongPreference else Definitions.WeakPreference
    } yield PersonTopicPreference(p, t, str)
  }

  object Problems {
    val Complete = {
      val p = Problem(Slots.All, Topics.All, Persons.All, Constraints.All, Preferences.All)
      log.debug(s"Problem is $p")
      p
    }
  }

}


