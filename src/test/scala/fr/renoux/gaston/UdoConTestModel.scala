package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.input.InputSettings
import fr.renoux.gaston.model.Schedule.Record
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, Preference}
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
    incompatibilityAntiPreference = Score(-50),
    defaultMin = 3,
    defaultMax = 5
  )

  object Persons {
    //private val nicknames = Set("Mangon", "Sammael99", "Zeben", "Kamiseito", "Kersa", "Tolkraft", "Cryoban", "Paradoks", "Paiji", "Isidore", "Gabzeta", "Highlandjul", "Orfeo", "Rolapin", "Ozen", "Bashar", "Selpoivre", "Chestel", "Jorune", "Aude", "Killerklown", "Najael", "Eugénie", "Julian", "Goat", "Boojum", "Udo Femi")
    val All: Set[Person] = Set(Person("Najael", Weight(1.0)), Person("Sammael99", Weight(1.5)), Person("Jorune", Weight(1.5)), Person("Paradoks", Weight(1.5)), Person("Chestel", Weight(1.0)), Person("Eugénie", Weight(1.5)), Person("Zeben", Weight(1.5)), Person("Orfeo", Weight(1.5)), Person("Kersa", Weight(1.0)), Person("Selpoivre", Weight(1.5)), Person("Kamiseito", Weight(1.0)), Person("Highlandjul", Weight(1.5)), Person("Gabzeta", Weight(1.5)), Person("Isidore", Weight(1.0)), Person("Paiji", Weight(1.0)), Person("Cryoban", Weight(1.5)), Person("Bashar", Weight(1.0)), Person("Killerklown", Weight(1.5)), Person("Goat", Weight(1.5)), Person("Tolkraft", Weight(1.5)), Person("Aude", Weight(1.0)), Person("Udo Femi", Weight(1.5)), Person("Ozen", Weight(1.5)), Person("Rolapin", Weight(1.5)), Person("Mangon", Weight(1.0)), Person("Julian", Weight(1.5)), Person("Boojum", Weight(1.5)))
    val byName: Map[String, Person] = All.map(p => p.name -> p).toMap
  }

  object Topics {
    private val topicNames = Set("Agôn", "DC comics - Darkest night", "Les Schtroumpfs", "Shadow of the Demon Lord", "Delta Green - Scénario original", "Blades in the Dark", "Skyrealms of Jorune Revival", "Wurm : Rouge massacre", "Psi*run", "Coriolis: Third horizon", "Mexican Death trip", "Meute", "Les Derniers", "Chiens de guerre", "Tales from the Loop", "Donjon & Cie", "P.U.N.C.H Unit - Katanga 1960", "Delta Green - Pennsylvania '99", "Inflorenza (ambiance Patient 13)", "KPDP dans le Dodécaèdre", "Summer camp - classe de neige", "Dieux Ennemis", "Wastburg", "Black Mirror", "Le Retour de Soth", "Godbound - l'appel")
    private val additionalTopicNames = Set("Burning Wheel", "end of line", "Aux Marches du Pouvoir", "DCC Funnel - Ferme des Célébrités", "Tortues Ninja (Fate)", "Héroïques")
    val Selected: Set[Topic] = topicNames map Topic
    val All: Set[Topic] = Selected ++ (additionalTopicNames map Topic)
    val byName: Map[String, Topic] = All.map(p => p.name -> p).toMap
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
      Set(PersonTopicInterdiction(topic = Topic("Le Retour de Soth"), person = Persons.byName("Tolkraft"))
      )

    val AllInterdictions: Set[Constraint] = SelectedInterdictions ++ Set()

    val Absences: Set[Constraint] = Set(PersonAbsence(Persons.byName("Ozen"), Slot("D3-afternoon")))

    val SelectedNumbers: Set[Constraint] =
      Set(
        TopicNeedsNumberOfPersons(Topic("Agôn"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("DC comics - Darkest night"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Les Schtroumpfs"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Shadow of the Demon Lord"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Delta Green - Scénario original"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Blades in the Dark"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Skyrealms of Jorune Revival"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Wurm : Rouge massacre"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Psi*run"), min = 4, max = 5),
        TopicNeedsNumberOfPersons(Topic("Coriolis: Third horizon"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Mexican Death trip"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Meute"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Les Derniers"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Chiens de guerre"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Tales from the Loop"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Donjon & Cie"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("P.U.N.C.H Unit - Katanga 1960"), min = 4, max = 5),
        TopicNeedsNumberOfPersons(Topic("Delta Green - Pennsylvania '99"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Inflorenza (ambiance Patient 13)"), min = 4, max = 5),
        TopicNeedsNumberOfPersons(Topic("KPDP dans le Dodécaèdre"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Summer camp - classe de neige"), min = 4, max = 7),
        TopicNeedsNumberOfPersons(Topic("Dieux Ennemis"), min = 4, max = 7),
        TopicNeedsNumberOfPersons(Topic("Wastburg"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Black Mirror"), min = 4, max = 5),
        TopicNeedsNumberOfPersons(Topic("Le Retour de Soth"), min = 4, max = 5),
        TopicNeedsNumberOfPersons(Topic("Godbound - l'appel"), min = 4, max = 5)
      )

    val AllNumbers: Set[Constraint] = SelectedNumbers ++
      Set(
        TopicNeedsNumberOfPersons(Topic("Burning Wheel"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("end of line"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Aux Marches du Pouvoir"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("DCC Funnel - Ferme des Célébrités"), min = 4, max = 6),
        TopicNeedsNumberOfPersons(Topic("Tortues Ninja (Fate)"), min = 5, max = 5),
        TopicNeedsNumberOfPersons(Topic("Héroïques"), min = 4, max = 6)
      )

    val Selected: Set[Constraint] = SelectedObligations ++ SelectedInterdictions ++ Absences ++ SelectedNumbers
    val All: Set[Constraint] = AllObligations ++ AllInterdictions ++ Absences ++ AllNumbers
  }

  object Preferences {
    private val preferenceTriplets = Set(
      ("Aude", "Delta Green - Pennsylvania '99", 1.0),
      ("Aude", "Delta Green - Scénario original", 1.0),
      ("Aude", "Donjon & Cie", 5.0),
      ("Aude", "Godbound - l'appel", 1.0),
      ("Aude", "Inflorenza (ambiance Patient 13)", 1.0),
      ("Aude", "Les Schtroumpfs", 1.0),
      ("Aude", "Mexican Death trip", 1.0),
      ("Aude", "Shadow of the Demon Lord", 1.0),
      ("Aude", "Tales from the Loop", 5.0),
      ("Aude", "Wastburg", 5.0),
      ("Aude", "Wurm : Rouge massacre", 1.0),
      ("Bashar", "Agôn", 5.0),
      ("Bashar", "DC comics - Darkest night", 1.0),
      ("Bashar", "Delta Green - Pennsylvania '99", 1.0),
      ("Bashar", "Delta Green - Scénario original", 1.0),
      ("Bashar", "Dieux Ennemis", 1.0),
      ("Bashar", "Godbound - l'appel", 1.0),
      ("Bashar", "Le Retour de Soth", 1.0),
      ("Bashar", "P.U.N.C.H Unit - Katanga 1960", 1.0),
      ("Bashar", "Psi*run", 1.0),
      ("Bashar", "Tales from the Loop", 5.0),
      ("Boojum", "Black Mirror", 5.0),
      ("Boojum", "DC comics - Darkest night", 1.0),
      ("Boojum", "Donjon & Cie", 5.0),
      ("Boojum", "Godbound - l'appel", 1.0),
      ("Boojum", "KPDP dans le Dodécaèdre", 1.0),
      ("Boojum", "Mexican Death trip", 1.0),
      ("Boojum", "Shadow of the Demon Lord", 1.0),
      ("Boojum", "Tales from the Loop", 5.0),
      ("Chestel", "DC comics - Darkest night", 5.0),
      ("Chestel", "Delta Green - Scénario original", 1.0),
      ("Chestel", "Inflorenza (ambiance Patient 13)", 5.0),
      ("Chestel", "Le Retour de Soth", 1.0),
      ("Chestel", "Les Derniers", 1.0),
      ("Chestel", "Skyrealms of Jorune Revival", 5.0),
      ("Chestel", "Summer camp - classe de neige", 1.0),
      ("Chestel", "Tales from the Loop", 1.0),
      ("Cryoban", "Blades in the Dark", 1.0),
      ("Cryoban", "Coriolis: Third horizon", 1.0),
      ("Cryoban", "DC comics - Darkest night", 5.0),
      ("Cryoban", "Donjon & Cie", 1.0),
      ("Cryoban", "Les Schtroumpfs", 1.0),
      ("Cryoban", "Meute", 5.0),
      ("Cryoban", "Wastburg", 5.0),
      ("Cryoban", "Wurm : Rouge massacre", 1.0),
      ("Eugénie", "Chiens de guerre", 5.0),
      ("Eugénie", "Delta Green - Scénario original", 1.0),
      ("Eugénie", "KPDP dans le Dodécaèdre", 5.0),
      ("Eugénie", "Psi*run", 1.0),
      ("Eugénie", "Tales from the Loop", 1.0),
      ("Eugénie", "Wastburg", 1.0),
      ("Gabzeta", "Black Mirror", 1.0),
      ("Gabzeta", "Coriolis: Third horizon", 1.0),
      ("Gabzeta", "Donjon & Cie", 1.0),
      ("Gabzeta", "Godbound - l'appel", 1.0),
      ("Gabzeta", "Inflorenza (ambiance Patient 13)", 1.0),
      ("Gabzeta", "KPDP dans le Dodécaèdre", 1.0),
      ("Gabzeta", "Le Retour de Soth", 1.0),
      ("Gabzeta", "Les Schtroumpfs", 5.0),
      ("Gabzeta", "Meute", 5.0),
      ("Gabzeta", "Mexican Death trip", 1.0),
      ("Gabzeta", "Shadow of the Demon Lord", 1.0),
      ("Gabzeta", "Summer camp - classe de neige", 1.0),
      ("Gabzeta", "Tales from the Loop", 1.0),
      ("Gabzeta", "Wastburg", 5.0),
      ("Goat", "Blades in the Dark", 5.0),
      ("Goat", "Dieux Ennemis", 1.0),
      ("Goat", "Godbound - l'appel", 5.0),
      ("Goat", "Shadow of the Demon Lord", 5.0),
      ("Highlandjul", "Black Mirror", 1.0),
      ("Highlandjul", "Blades in the Dark", 1.0),
      ("Highlandjul", "DC comics - Darkest night", 1.0),
      ("Highlandjul", "Donjon & Cie", 5.0),
      ("Highlandjul", "Inflorenza (ambiance Patient 13)", 1.0),
      ("Highlandjul", "Les Derniers", 1.0),
      ("Highlandjul", "Meute", 5.0),
      ("Highlandjul", "Summer camp - classe de neige", 1.0),
      ("Highlandjul", "Wastburg", 1.0),
      ("Isidore", "Blades in the Dark", 1.0),
      ("Isidore", "Chiens de guerre", 5.0),
      ("Isidore", "Delta Green - Pennsylvania '99", 1.0),
      ("Isidore", "Dieux Ennemis", 1.0),
      ("Isidore", "Donjon & Cie", 1.0),
      ("Isidore", "Le Retour de Soth", 1.0),
      ("Isidore", "Les Schtroumpfs", 1.0),
      ("Isidore", "Meute", 1.0),
      ("Isidore", "Psi*run", 1.0),
      ("Isidore", "Skyrealms of Jorune Revival", 5.0),
      ("Isidore", "Tales from the Loop", 5.0),
      ("Jorune", "Black Mirror", 5.0),
      ("Jorune", "Chiens de guerre", 1.0),
      ("Jorune", "DC comics - Darkest night", 5.0),
      ("Jorune", "Donjon & Cie", 1.0),
      ("Jorune", "Godbound - l'appel", 1.0),
      ("Jorune", "Les Derniers", 1.0),
      ("Jorune", "Les Schtroumpfs", 1.0),
      ("Jorune", "Shadow of the Demon Lord", 1.0),
      ("Jorune", "Skyrealms of Jorune Revival", 5.0),
      ("Jorune", "Wurm : Rouge massacre", 1.0),
      ("Julian", "Blades in the Dark", 1.0),
      ("Julian", "Coriolis: Third horizon", 1.0),
      ("Julian", "DC comics - Darkest night", 1.0),
      ("Julian", "Donjon & Cie", 1.0),
      ("Julian", "Les Derniers", 1.0),
      ("Julian", "Psi*run", 1.0),
      ("Kamiseito", "Agôn", 5.0),
      ("Kamiseito", "Dieux Ennemis", 1.0),
      ("Kamiseito", "Donjon & Cie", 1.0),
      ("Kamiseito", "Les Derniers", 5.0),
      ("Kamiseito", "Meute", 1.0),
      ("Kamiseito", "Mexican Death trip", 1.0),
      ("Kamiseito", "P.U.N.C.H Unit - Katanga 1960", 1.0),
      ("Kamiseito", "Tales from the Loop", 5.0),
      ("Kersa", "Chiens de guerre", 1.0),
      ("Kersa", "Inflorenza (ambiance Patient 13)", 1.0),
      ("Kersa", "KPDP dans le Dodécaèdre", 1.0),
      ("Kersa", "Le Retour de Soth", 1.0),
      ("Kersa", "Les Derniers", 5.0),
      ("Kersa", "Meute", 1.0),
      ("Kersa", "P.U.N.C.H Unit - Katanga 1960", 5.0),
      ("Kersa", "Psi*run", 1.0),
      ("Kersa", "Wastburg", 5.0),
      ("Killerklown", "Black Mirror", 1.0),
      ("Killerklown", "Blades in the Dark", 1.0),
      ("Killerklown", "Chiens de guerre", 1.0),
      ("Killerklown", "Delta Green - Pennsylvania '99", 1.0),
      ("Killerklown", "Donjon & Cie", 1.0),
      ("Killerklown", "KPDP dans le Dodécaèdre", 1.0),
      ("Killerklown", "Le Retour de Soth", 1.0),
      ("Killerklown", "Meute", 5.0),
      ("Killerklown", "Mexican Death trip", 1.0),
      ("Killerklown", "P.U.N.C.H Unit - Katanga 1960", 1.0),
      ("Killerklown", "Skyrealms of Jorune Revival", 1.0),
      ("Killerklown", "Tales from the Loop", 5.0),
      ("Killerklown", "Wastburg", 1.0),
      ("Mangon", "Chiens de guerre", 1.0),
      ("Mangon", "Delta Green - Pennsylvania '99", 1.0),
      ("Mangon", "Donjon & Cie", 5.0),
      ("Mangon", "P.U.N.C.H Unit - Katanga 1960", 1.0),
      ("Mangon", "Skyrealms of Jorune Revival", 5.0),
      ("Najael", "Blades in the Dark", 1.0),
      ("Najael", "Coriolis: Third horizon", 1.0),
      ("Najael", "Delta Green - Scénario original", 1.0),
      ("Najael", "Donjon & Cie", 1.0),
      ("Najael", "KPDP dans le Dodécaèdre", 5.0),
      ("Najael", "Mexican Death trip", 5.0),
      ("Najael", "Tales from the Loop", 5.0),
      ("Najael", "Wastburg", 1.0),
      ("Orfeo", "Blades in the Dark", 5.0),
      ("Orfeo", "Chiens de guerre", 1.0),
      ("Orfeo", "Coriolis: Third horizon", 1.0),
      ("Orfeo", "Inflorenza (ambiance Patient 13)", 5.0),
      ("Orfeo", "KPDP dans le Dodécaèdre", 1.0),
      ("Orfeo", "Mexican Death trip", 1.0),
      ("Orfeo", "Shadow of the Demon Lord", 1.0),
      ("Orfeo", "Wurm : Rouge massacre", 1.0),
      ("Ozen", "Agôn", 5.0),
      ("Ozen", "Black Mirror", 5.0),
      ("Ozen", "Dieux Ennemis", 1.0),
      ("Ozen", "Donjon & Cie", 1.0),
      ("Ozen", "Les Derniers", 1.0),
      ("Ozen", "Psi*run", 5.0),
      ("Paiji", "Blades in the Dark", 1.0),
      ("Paiji", "Coriolis: Third horizon", 1.0),
      ("Paiji", "Delta Green - Pennsylvania '99", 5.0),
      ("Paiji", "Donjon & Cie", 1.0),
      ("Paiji", "Godbound - l'appel", 1.0),
      ("Paiji", "KPDP dans le Dodécaèdre", 5.0),
      ("Paiji", "Les Derniers", 1.0),
      ("Paiji", "P.U.N.C.H Unit - Katanga 1960", 1.0),
      ("Paiji", "Shadow of the Demon Lord", 1.0),
      ("Paiji", "Skyrealms of Jorune Revival", 5.0),
      ("Paiji", "Wastburg", 1.0),
      ("Paradoks", "Blades in the Dark", 5.0),
      ("Paradoks", "Coriolis: Third horizon", 1.0),
      ("Paradoks", "Godbound - l'appel", 1.0),
      ("Paradoks", "Inflorenza (ambiance Patient 13)", 5.0),
      ("Paradoks", "Les Derniers", 1.0),
      ("Paradoks", "Mexican Death trip", 5.0),
      ("Paradoks", "Tales from the Loop", 1.0),
      ("Paradoks", "Wastburg", 1.0),
      ("Paradoks", "Wurm : Rouge massacre", 1.0),
      ("Rolapin", "Coriolis: Third horizon", 1.0),
      ("Rolapin", "Delta Green - Pennsylvania '99", 1.0),
      ("Rolapin", "Godbound - l'appel", 1.0),
      ("Rolapin", "Mexican Death trip", 5.0),
      ("Rolapin", "P.U.N.C.H Unit - Katanga 1960", 1.0),
      ("Rolapin", "Psi*run", 1.0),
      ("Rolapin", "Shadow of the Demon Lord", 5.0),
      ("Rolapin", "Skyrealms of Jorune Revival", 1.0),
      ("Rolapin", "Summer camp - classe de neige", 1.0),
      ("Rolapin", "Tales from the Loop", 1.0),
      ("Rolapin", "Wastburg", 1.0),
      ("Sammael99", "Coriolis: Third horizon", 5.0),
      ("Sammael99", "Delta Green - Scénario original", 1.0),
      ("Sammael99", "Dieux Ennemis", 1.0),
      ("Sammael99", "Godbound - l'appel", 5.0),
      ("Sammael99", "Les Schtroumpfs", 1.0),
      ("Sammael99", "Meute", 5.0),
      ("Sammael99", "Summer camp - classe de neige", 1.0),
      ("Sammael99", "Wastburg", 1.0),
      ("Sammael99", "Wurm : Rouge massacre", 1.0),
      ("Selpoivre", "Black Mirror", 1.0),
      ("Selpoivre", "Blades in the Dark", 1.0),
      ("Selpoivre", "Coriolis: Third horizon", 1.0),
      ("Selpoivre", "Donjon & Cie", 1.0),
      ("Selpoivre", "Inflorenza (ambiance Patient 13)", 1.0),
      ("Selpoivre", "Les Derniers", 1.0),
      ("Selpoivre", "Meute", 1.0),
      ("Selpoivre", "Mexican Death trip", 1.0),
      ("Selpoivre", "Tales from the Loop", 5.0),
      ("Selpoivre", "Wastburg", 5.0),
      ("Selpoivre", "Wurm : Rouge massacre", 1.0),
      ("Tolkraft", "Chiens de guerre", 1.0),
      ("Tolkraft", "Delta Green - Pennsylvania '99", 5.0),
      ("Tolkraft", "Donjon & Cie", 1.0),
      ("Tolkraft", "Inflorenza (ambiance Patient 13)", 1.0),
      ("Tolkraft", "Meute", 1.0),
      ("Tolkraft", "Mexican Death trip", 1.0),
      ("Tolkraft", "Shadow of the Demon Lord", 1.0),
      ("Tolkraft", "Wastburg", 5.0),
      ("Zeben", "Agôn", 1.0),
      ("Zeben", "Blades in the Dark", 5.0),
      ("Zeben", "KPDP dans le Dodécaèdre", 5.0),
      ("Zeben", "Meute", 1.0),
      ("Zeben", "Psi*run", 1.0),
      ("Zeben", "Tales from the Loop", 5.0),
      ("Zeben", "Wastburg", 1.0),
      ("Udo Femi", "Agôn", 1.0),
      ("Udo Femi", "Black Mirror", 5.0),
      ("Udo Femi", "Blades in the Dark", 5.0),
      ("Udo Femi", "Delta Green - Scénario original", 1.0),
      ("Udo Femi", "Donjon & Cie", 1.0),
      ("Udo Femi", "KPDP dans le Dodécaèdre", 1.0),
      ("Udo Femi", "Le Retour de Soth", 1.0),
      ("Udo Femi", "Les Schtroumpfs", 1.0),
      ("Udo Femi", "Shadow of the Demon Lord", 1.0),
      ("Udo Femi", "Skyrealms of Jorune Revival", 5.0),
      ("Udo Femi", "Summer camp - classe de neige", 1.0)
    )

    val Selected: Set[Preference] = preferenceTriplets map { case (pName, tName, scoreValue) =>
      PersonTopicPreference(Persons.byName(pName), Topics.byName(tName), Score(scoreValue))
    }

    val All: Set[Preference] = Selected
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

  object Solutions {
    import Persons.byName
    val Actual = Schedule(5,
      Record(Slot("D1-afternoon"), Topic("Agôn"),                             Set[Person]( byName("Mangon"), byName("Kamiseito"), byName("Highlandjul"), byName("Ozen"), byName("Bashar")) ),
      Record(Slot("D1-afternoon"), Topic("DC comics - Darkest night"),        Set[Person]( byName("Cryoban"), byName("Selpoivre"), byName("Chestel"), byName("Jorune"), byName("Boojum")) ),
      Record(Slot("D1-afternoon"), Topic("Les Schtroumpfs"),                  Set[Person]( byName("Paradoks"), byName("Isidore"), byName("Gabzeta"), byName("Aude"), byName("Julian")) ),
      Record(Slot("D1-afternoon"), Topic("Shadow of the Demon Lord"),         Set[Person]( byName("Zeben"), byName("Paiji"), byName("Orfeo"), byName("Rolapin"), byName("Goat"), byName("Udo Femi")) ),
      Record(Slot("D1-afternoon"), Topic("Delta Green - Scénario original"),  Set[Person]( byName("Sammael99"), byName("Kersa"), byName("Tolkraft"), byName("Killerklown"), byName("Najael"), byName("Eugénie")) ),
      Record(Slot("D1-evening"), Topic("Blades in the Dark"),                 Set[Person]( byName("Zeben"), byName("Highlandjul"), byName("Orfeo"), byName("Rolapin"), byName("Goat"), byName("Udo Femi")) ),
      Record(Slot("D1-evening"), Topic("Skyrealms of Jorune Revival"),        Set[Person]( byName("Mangon"), byName("Sammael99"), byName("Paiji"), byName("Isidore"), byName("Chestel"), byName("Jorune")) ),
      Record(Slot("D1-evening"), Topic("Wurm : Rouge massacre"),              Set[Person]( byName("Kamiseito"), byName("Cryoban"), byName("Gabzeta"), byName("Selpoivre"), byName("Aude")) ),
      Record(Slot("D1-evening"), Topic("Psi*run"),                            Set[Person]( byName("Kersa"), byName("Paradoks"), byName("Ozen"), byName("Bashar"), byName("Eugénie")) ),
      Record(Slot("D1-evening"), Topic("Coriolis: Third horizon"),            Set[Person]( byName("Tolkraft"), byName("Killerklown"), byName("Najael"), byName("Julian"), byName("Boojum")) ),
      Record(Slot("D2-afternoon"), Topic("Mexican Death trip"),               Set[Person]( byName("Paradoks"), byName("Rolapin"), byName("Ozen"), byName("Najael"), byName("Julian")) ),
      Record(Slot("D2-afternoon"), Topic("Meute"),                            Set[Person]( byName("Sammael99"), byName("Cryoban"), byName("Gabzeta"), byName("Highlandjul"), byName("Jorune"), byName("Killerklown")) ),
      Record(Slot("D2-afternoon"), Topic("Les Derniers"),                     Set[Person]( byName("Kamiseito"), byName("Kersa"), byName("Tolkraft"), byName("Paiji"), byName("Chestel")) ),
      Record(Slot("D2-afternoon"), Topic("Chiens de guerre"),                 Set[Person]( byName("Mangon"), byName("Isidore"), byName("Orfeo"), byName("Eugénie"), byName("Goat")) ),
      Record(Slot("D2-afternoon"), Topic("Tales from the Loop"),              Set[Person]( byName("Zeben"), byName("Bashar"), byName("Selpoivre"), byName("Aude"), byName("Boojum"), byName("Udo Femi")) ),
      Record(Slot("D2-evening"), Topic("Donjon & Cie"),                       Set[Person]( byName("Mangon"), byName("Sammael99"), byName("Highlandjul"), byName("Aude"), byName("Julian"), byName("Boojum")) ),
      Record(Slot("D2-evening"), Topic("P.U.N.C.H Unit - Katanga 1960"),      Set[Person]( byName("Kamiseito"), byName("Kersa"), byName("Cryoban"), byName("Bashar"), byName("Killerklown")) ),
      Record(Slot("D2-evening"), Topic("Delta Green - Pennsylvania '99"),     Set[Person]( byName("Tolkraft"), byName("Paiji"), byName("Rolapin"), byName("Isidore"), byName("Selpoivre"), byName("Jorune")) ),
      Record(Slot("D2-evening"), Topic("Inflorenza (ambiance Patient 13)"),   Set[Person]( byName("Paradoks"), byName("Gabzeta"), byName("Orfeo"), byName("Chestel"), byName("Eugénie")) ),
      Record(Slot("D2-evening"), Topic("KPDP dans le Dodécaèdre"),            Set[Person]( byName("Zeben"), byName("Ozen"), byName("Najael"), byName("Goat"), byName("Udo Femi")) ),
      Record(Slot("D3-afternoon"), Topic("Summer camp - classe de neige"),    Set[Person]( byName("Paradoks"), byName("Chestel"), byName("Eugénie"), byName("Julian")) ),
      Record(Slot("D3-afternoon"), Topic("Dieux Ennemis"),                    Set[Person]( byName("Zeben"), byName("Kamiseito"), byName("Highlandjul"), byName("Bashar")) ),
      Record(Slot("D3-afternoon"), Topic("Wastburg"),                         Set[Person]( byName("Kersa"), byName("Tolkraft"), byName("Gabzeta"), byName("Selpoivre"), byName("Aude"), byName("Goat")) ),
      Record(Slot("D3-afternoon"), Topic("Black Mirror"),                     Set[Person]( byName("Mangon"), byName("Orfeo"), byName("Jorune"), byName("Udo Femi")) ),
      Record(Slot("D3-afternoon"), Topic("Le Retour de Soth"),                Set[Person]( byName("Isidore"), byName("Killerklown"), byName("Najael"), byName("Boojum")) ),
      Record(Slot("D3-afternoon"), Topic("Godbound - l'appel"),               Set[Person]( byName("Sammael99"), byName("Cryoban"), byName("Paiji"), byName("Rolapin")) )
    )
  }

}


