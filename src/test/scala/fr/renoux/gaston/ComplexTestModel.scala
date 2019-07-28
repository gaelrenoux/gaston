package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Score.ScoreIsFractional._
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.util.RandomImplicits._

import scala.collection.mutable
import scala.util.Random

// scalastyle:off magic.number

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
    val All: Set[Topic] = topicNames.map { n =>
      val mand = random.pick(Persons.All)
      val forb = random.pick(Persons.All - mand)
      Topic(n, mandatory = Set(mand), forbidden = Set(forb), min = 4, max = 12)
    }
    val Bases: Set[Topic] = Slots.AllSet.map(s => Topic.unassigned(s))
  }

  object Slots {
    private val slotNames = Seq(Seq("d1 am", "d1 pm"), Seq("d2 am", "d2 pm"), Seq("d3 am", "d3 pm"))
    val AllSequence: Seq[Seq[Slot]] = slotNames.map(_.map(Slot(_, Persons.All -- random.pick(Persons.All, 2))))
    val AllSet: Set[Slot] = AllSequence.flatten.toSet
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

    val All: Set[Preference] = PersonTopics ++ Incompatibilities // + PersonTopicPreference(Person("brigit kevin"),
    // Topic("sigma"), Score(100))

    val Bases: Set[Preference] = for {
      t <- Topics.Bases
      p <- Persons.All
    } yield PersonTopicPreference(p, t, Score.PersonTotalScore.negative)
  }

  object Problems {
    val Complete: Problem = {
      val p = new ProblemImpl(
        Slots.AllSequence,
        Topics.All ++ Topics.Bases,
        Persons.All,
        Set.empty,
        Preferences.All ++ Preferences.Bases
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
