package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.InputSettings
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.Preference
import fr.renoux.gaston.model.problem.Problem

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
object UdoConTestModel {

  private val log = Logger(UdoConTestModel.getClass)

  implicit val random: Random = new Random(0L)

  val Settings = InputSettings(
    weakPreference = Score(1),
    strongPreference = Score(5),
    incompatibilityAntiPreference = Score(-50)
  )

  object Persons {
    private val nicknames = Set("Mangon", "Sammael99", "Zeben", "Kamiseito", "Kersa", "Tolkraft", "Cryoban", "Paradoks", "Paiji", "Isidore", "Gabzeta", "Highlandjul", "Orfeo", "Rolapin", "Ozen", "Bashar", "Selpoivre", "Chestel", "Jorune", "Aude", "Killerklown", "Najael", "Eugénie", "Julian", "Goat", "Boojum", "Udo Femi")
    val All: Set[Person] = nicknames map {
      case n if n.head == 'A' || n.head == 'B' => Person(n, Weight(2))
      case n => Person(n, Weight(1))
    }
    val byName: Map[String, Person] = All.map(p => p.name -> p).toMap
  }

  object Topics {
    private val topicNames = Set("Agôn", "DC comics - Darkest night", "Les Schtroumpfs", "Shadow of the Demon Lord", "Delta Green - Scénario original", "Blades in the Dark", "Skyrealms of Jorune Revival", "Wurm : Rouge massacre", "Psi*run", "Coriolis: Third horizon", "Mexican Death trip", "Meute", "Les Derniers", "Chiens de guerre", "Tales from the Loop", "Donjon & Cie", "P.U.N.C.H Unit - Katanga 1960", "Delta Green - Pennsylvania '99", "Inflorenza (ambiance Patient 13)", "KPDP dans le Dodécaèdre", "Summer camp - classe de neige", "Dieux Ennemis", "Wastburg", "Black Mirror", "Le Retour de Soth", "Godbound - l'appel")
    private val additionalTopicNames = Set("Burning Wheel", "end of line", "Aux Marches du Pouvoir", "DCC Funnel - Ferme des Célébrités", "Tortues Ninja (Fate)", "Héroïques")
    val Selected: Set[Topic] = topicNames map Topic
    val All: Set[Topic] = Selected ++ (additionalTopicNames map Topic)
  }

  object Slots {
    private val slotNames = Set("D1-afternoon", "D1-evening", "D2-afternoon", "D2-evening", "D3-afternoon")
    val All: Set[Slot] = slotNames map Slot
  }

  object Constraints {

    val SelectedObligations: Set[Constraint] =
      Set(PersonTopicObligation(topic = Topic("Agôn"), person = Persons.byName("Highlandjul")),
        PersonTopicObligation(topic = Topic("DC comics - Darkest night"), person = Persons.byName("Selpoivre")),
        PersonTopicObligation(topic = Topic("Les Schtroumpfs"), person = Persons.byName("Julian")),
        PersonTopicObligation(topic = Topic("Shadow of the Demon Lord"), person = Persons.byName("Zeben")),
        PersonTopicObligation(topic = Topic("Delta Green - Scénario original"), person = Persons.byName("Tolkraft")),
        PersonTopicObligation(topic = Topic("Blades in the Dark"), person = Persons.byName("Rolapin")),
        PersonTopicObligation(topic = Topic("Skyrealms of Jorune Revival"), person = Persons.byName("Sammael99")),
        PersonTopicObligation(topic = Topic("Wurm : Rouge massacre"), person = Persons.byName("Gabzeta")),
        PersonTopicObligation(topic = Topic("Psi*run"), person = Persons.byName("Paradoks")),
        PersonTopicObligation(topic = Topic("Coriolis: Third horizon"), person = Persons.byName("Killerklown")),
        PersonTopicObligation(topic = Topic("Mexican Death trip"), person = Persons.byName("Ozen")),
        PersonTopicObligation(topic = Topic("Meute"), person = Persons.byName("Jorune")),
        PersonTopicObligation(topic = Topic("Les Derniers"), person = Persons.byName("Tolkraft")),
        PersonTopicObligation(topic = Topic("Chiens de guerre"), person = Persons.byName("Goat")),
        PersonTopicObligation(topic = Topic("Tales from the Loop"), person = Persons.byName("Udo Femi")),
        PersonTopicObligation(topic = Topic("Donjon & Cie"), person = Persons.byName("Sammael99")),
        PersonTopicObligation(topic = Topic("P.U.N.C.H Unit - Katanga 1960"), person = Persons.byName("Cryoban")),
        PersonTopicObligation(topic = Topic("Delta Green - Pennsylvania '99"), person = Persons.byName("Selpoivre")),
        PersonTopicObligation(topic = Topic("Inflorenza (ambiance Patient 13)"), person = Persons.byName("Eugénie")),
        PersonTopicObligation(topic = Topic("KPDP dans le Dodécaèdre"), person = Persons.byName("Ozen")),
        PersonTopicObligation(topic = Topic("Summer camp - classe de neige"), person = Persons.byName("Paradoks")),
        PersonTopicObligation(topic = Topic("Dieux Ennemis"), person = Persons.byName("Highlandjul")),
        PersonTopicObligation(topic = Topic("Wastburg"), person = Persons.byName("Goat")),
        PersonTopicObligation(topic = Topic("Black Mirror"), person = Persons.byName("Orfeo")),
        PersonTopicObligation(topic = Topic("Le Retour de Soth"), person = Persons.byName("Boojum")),
        PersonTopicObligation(topic = Topic("Godbound - l'appel"), person = Persons.byName("Cryoban")),
      )

    val AllObligations: Set[Constraint] = SelectedObligations ++
      Set(PersonTopicObligation(topic = Topic("Burning Wheel"), person = Persons.byName("Udo Femi")),
        PersonTopicObligation(topic = Topic("end of line"), person = Persons.byName("Julian")),
        PersonTopicObligation(topic = Topic("Aux Marches du Pouvoir"), person = Persons.byName("Paradoks")),
        PersonTopicObligation(topic = Topic("DCC Funnel - Ferme des Célébrités"), person = Persons.byName("Sammael99")),
        PersonTopicObligation(topic = Topic("Tortues Ninja (Fate)"), person = Persons.byName("Boojum")),
        PersonTopicObligation(topic = Topic("Héroïques"), person = Persons.byName("Kamiseito"))
      )

    val SelectedInterdictions: Set[Constraint] =
      Set(PersonTopicInterdiction(topic = Topic("Le Retour de Soth"), person = Persons.byName("Tolkraft")),
        PersonTopicInterdiction(topic = Topic("Le Retour de Soth"), person = Persons.byName("Selpoivre"))
      )

    val AllInterdictions: Set[Constraint] = SelectedInterdictions ++
      Set()

    val Absences: Set[Constraint] = Set()

    val SelectedNumbers: Set[Constraint] =
      Set(TopicNeedsNumberOfPersons(Topic("Agôn"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("DC comics - Darkest night"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Les Schtroumpfs"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Shadow of the Demon Lord"), min = 3, max = 4),
        TopicNeedsNumberOfPersons(Topic("Delta Green - Scénario original"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Blades in the Dark"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Skyrealms of Jorune Revival"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Wurm : Rouge massacre"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Psi*run"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Coriolis: Third horizon"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Mexican Death trip"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Meute"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Les Derniers"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Chiens de guerre"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Tales from the Loop"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Donjon & Cie"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("P.U.N.C.H Unit - Katanga 1960"), min = 3, max = 4),
        TopicNeedsNumberOfPersons(Topic("Delta Green - Pennsylvania '99"), min = 3, max = 4),
        TopicNeedsNumberOfPersons(Topic("Inflorenza (ambiance Patient 13)"), min = 3, max = 4),
        TopicNeedsNumberOfPersons(Topic("KPDP dans le Dodécaèdre"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Summer camp - classe de neige"), min = 3, max = 6),
        TopicNeedsNumberOfPersons(Topic("Dieux Ennemis"), min = 3, max = 6),
        TopicNeedsNumberOfPersons(Topic("Wastburg"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Black Mirror"), min = 3, max = 4),
        TopicNeedsNumberOfPersons(Topic("Le Retour de Soth"), min = 3, max = 4),
        TopicNeedsNumberOfPersons(Topic("Godbound - l'appel"), min = 3, max = 4)
      )

    val AllNumbers: Set[Constraint] = SelectedNumbers ++
      Set(TopicNeedsNumberOfPersons(Topic("Burning Wheel"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("end of line"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Aux Marches du Pouvoir"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("DCC Funnel - Ferme des Célébrités"), min = 3, max = 5),
        TopicNeedsNumberOfPersons(Topic("Tortues Ninja (Fate)"), min = 4, max = 4),
        TopicNeedsNumberOfPersons(Topic("Héroïques"), min = 3, max = 5)
      )

    val Selected: Set[Constraint] = SelectedObligations ++ SelectedInterdictions ++ Absences ++ SelectedNumbers
    val All: Set[Constraint] = AllObligations ++ AllInterdictions ++ Absences ++ AllNumbers
  }

  object Preferences {
    val Selected: Set[Preference] = Set()
    val All: Set[Preference] = Set()
  }

  object Problems {
    val Simplified = {
      val p = Problem(Slots.All, Topics.Selected, Persons.All, Constraints.Selected, Preferences.Selected)
      log.debug(s"Simplified problem is $p")
      p
    }
    val Complete = {
      val p = Problem(Slots.All, Topics.All, Persons.All, Constraints.All, Preferences.All)
      log.debug(s"Complete problem is $p")
      p
    }
  }

}


