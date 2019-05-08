package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.input.InputSettings
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.PersonTopicPreference
import fr.renoux.gaston.model.problem.ProblemImpl

import scala.util.Random

object UdoConTestModel {

  private val log = Logger(UdoConTestModel.getClass)

  implicit val random: Random = new Random(0L)

  val strongPreference = Score(5)
  val weakPreference = Score(1)

  val Settings = InputSettings(
    incompatibilityAntiPreference = Score(-50),
    defaultMinPersonsPerTopic = 3,
    defaultMaxPersonsPerTopic = 5
  )

  object Persons {
    val Najael = Person("Najael", Weight(1.0))
    val Sammael99 = Person("Sammael99", Weight(1.5))
    val Jorune = Person("Jorune", Weight(1.5))
    val Paradoks = Person("Paradoks", Weight(1.5))
    val Chestel = Person("Chestel", Weight(1.0))
    val Eugenie = Person("Eugénie", Weight(1.5))
    val Zeben = Person("Zeben", Weight(1.5))
    val Orfeo = Person("Orfeo", Weight(1.5))
    val Kersa = Person("Kersa", Weight(1.0))
    val Selpoivre = Person("Selpoivre", Weight(1.5))
    val Kamiseito = Person("Kamiseito", Weight(1.0))
    val Highlandjul = Person("Highlandjul", Weight(1.5))
    val Gabzeta = Person("Gabzeta", Weight(1.5))
    val Isidore = Person("Isidore", Weight(1.0))
    val Paiji = Person("Paiji", Weight(1.0))
    val Cryoban = Person("Cryoban", Weight(1.5))
    val Bashar = Person("Bashar", Weight(1.0))
    val Killerklown = Person("Killerklown", Weight(1.5))
    val Goat = Person("Goat", Weight(1.5))
    val Tolkraft = Person("Tolkraft", Weight(1.5))
    val Aude = Person("Aude", Weight(1.0))
    val UdoFemi = Person("Udo Femi", Weight(1.5))
    val Ozen = Person("Ozen", Weight(1.5))
    val Rolapin = Person("Rolapin", Weight(1.5))
    val Mangon = Person("Mangon", Weight(1.0))
    val Julian = Person("Julian", Weight(1.5))
    val Boojum = Person("Boojum", Weight(1.5))

    val All: Set[Person] = Set(
      Najael,
      Sammael99,
      Jorune,
      Paradoks,
      Chestel,
      Eugenie,
      Zeben,
      Orfeo,
      Kersa,
      Selpoivre,
      Kamiseito,
      Highlandjul,
      Gabzeta,
      Isidore,
      Paiji,
      Cryoban,
      Bashar,
      Killerklown,
      Goat,
      Tolkraft,
      Aude,
      UdoFemi,
      Ozen,
      Rolapin,
      Mangon,
      Julian,
      Boojum,
    )

    val byName: Map[String, Person] = All.map(p => p.name -> p).toMap
  }

  object Topics {
    val Agon = Topic("Agôn")
    val DcComics = Topic("DC comics - Darkest night")
    val LesSchtroumpfs = Topic("Les Schtroumpfs")
    val ShadowOfTheDemonLord = Topic("Shadow of the Demon Lord")
    val DeltaGreenOriginal = Topic("Delta Green - Scénario original")
    val BladesInTheDark = Topic("Blades in the Dark")
    val SkyrealmsOfJorune = Topic("Skyrealms of Jorune Revival")
    val Wurm = Topic("Wurm : Rouge massacre")
    val PsiRun = Topic("Psi*run")
    val Coriolis = Topic("Coriolis: Third horizon")
    val MexicanDeathTrip = Topic("Mexican Death trip")
    val Meute = Topic("Meute")
    val ChiensDeGuerre = Topic("Chiens de guerre")
    val TalesFromTheLoop = Topic("Tales from the Loop")
    val DonjonCie = Topic("Donjon & Cie")
    val Inflorenza = Topic("Inflorenza (ambiance Patient 13)")
    val PunchUnit = Topic("P.U.N.C.H Unit - Katanga 1960")
    val DeltaGreenPennsylvania = Topic("Delta Green - Pennsylvania '99")
    val LesDerniers = Topic("Les Derniers")
    val KpdpDansLeDodecaedre = Topic("KPDP dans le Dodécaèdre")
    val SummerCamp = Topic("Summer camp - classe de neige")
    val DieuxEnnemis = Topic("Dieux Ennemis")
    val Wastburg = Topic("Wastburg")
    val BlackMirror = Topic("Black Mirror")
    val LeRetourDeSoth = Topic("Le Retour de Soth")
    val Godbound = Topic("Godbound - l'appel")
    val BurningWheel = Topic("Burning Wheel")
    val EndOfLine = Topic("end of line")
    val AuxMarchesDuPouvoir = Topic("Aux Marches du Pouvoir")
    val DccFunnel = Topic("DCC Funnel - Ferme des Célébrités")
    val TortuesNinja = Topic("Tortues Ninja (Fate)")
    val Heroiques = Topic("Héroïques")

    val All: Set[Topic] = Set(
      Agon,
      DcComics,
      LesSchtroumpfs,
      ShadowOfTheDemonLord,
      DeltaGreenOriginal,
      BladesInTheDark,
      SkyrealmsOfJorune,
      Wurm,
      PsiRun,
      Coriolis,
      MexicanDeathTrip,
      Meute,
      ChiensDeGuerre,
      TalesFromTheLoop,
      DonjonCie,
      Inflorenza,
      PunchUnit,
      DeltaGreenPennsylvania,
      LesDerniers,
      KpdpDansLeDodecaedre,
      SummerCamp,
      DieuxEnnemis,
      Wastburg,
      BlackMirror,
      LeRetourDeSoth,
      Godbound,
      BurningWheel,
      EndOfLine,
      AuxMarchesDuPouvoir,
      DccFunnel,
      TortuesNinja,
      Heroiques
    )

    val byName: Map[String, Topic] = All.map(p => p.name -> p).toMap
  }

  object Slots {
    val D1Afternoon = Slot("D1-afternoon")
    val D1Evening = Slot("D1-evening")
    val D2Afternoon = Slot("D2-afternoon")
    val D2Evening = Slot("D2-evening")
    val D3Afternoon = Slot("D3-afternoon")

    val All: Seq[Seq[Slot]] = Seq(Seq(D1Afternoon, D1Evening), Seq(D2Afternoon, D2Evening), Seq(D3Afternoon))
  }

  object Constraints {

    import Persons._
    import Topics._
    import Slots._

    val AllObligations: Set[Constraint] = Set(
      PersonTopicObligation(Highlandjul, Agon),
      PersonTopicObligation(Selpoivre, DcComics),
      PersonTopicObligation(Julian, LesSchtroumpfs),
      PersonTopicObligation(Zeben, ShadowOfTheDemonLord),
      PersonTopicObligation(Tolkraft, DeltaGreenOriginal),
      PersonTopicObligation(Rolapin, BladesInTheDark),
      PersonTopicObligation(Sammael99, SkyrealmsOfJorune),
      PersonTopicObligation(Gabzeta, Wurm),
      PersonTopicObligation(Paradoks, PsiRun),
      PersonTopicObligation(Killerklown, Coriolis),
      PersonTopicObligation(Ozen, MexicanDeathTrip),
      PersonTopicObligation(Jorune, Meute),
      PersonTopicObligation(Tolkraft, LesDerniers),
      PersonTopicObligation(Goat, ChiensDeGuerre),
      PersonTopicObligation(UdoFemi, TalesFromTheLoop),
      PersonTopicObligation(Sammael99, DonjonCie),
      PersonTopicObligation(Cryoban, PunchUnit),
      PersonTopicObligation(Selpoivre, DeltaGreenPennsylvania),
      PersonTopicObligation(Eugenie, Inflorenza),
      PersonTopicObligation(Ozen, KpdpDansLeDodecaedre),
      PersonTopicObligation(Paradoks, SummerCamp),
      PersonTopicObligation(Highlandjul, DieuxEnnemis),
      PersonTopicObligation(Goat, Wastburg),
      PersonTopicObligation(Orfeo, BlackMirror),
      PersonTopicObligation(Boojum, LeRetourDeSoth),
      PersonTopicObligation(Cryoban, Godbound),
      PersonTopicObligation(UdoFemi, BurningWheel),
      PersonTopicObligation(Julian, EndOfLine),
      PersonTopicObligation(Paradoks, AuxMarchesDuPouvoir),
      PersonTopicObligation(Sammael99, DccFunnel),
      PersonTopicObligation(Boojum, TortuesNinja),
      PersonTopicObligation(Kamiseito, Heroiques)
    )

    val AllInterdictions: Set[Constraint] = Set(
      PersonTopicInterdiction(Tolkraft, LeRetourDeSoth)
    )

    val AllAbsences: Set[Constraint] = Set(
      PersonAbsence(Ozen, D3Afternoon)
    )

    val AllNumbers: Set[Constraint] =
      Set(
        TopicNeedsNumberOfPersons(Agon, min = 4, max = 6),
        TopicNeedsNumberOfPersons(DcComics, min = 4, max = 6),
        TopicNeedsNumberOfPersons(LesSchtroumpfs, min = 4, max = 6),
        TopicNeedsNumberOfPersons(ShadowOfTheDemonLord, min = 4, max = 6),
        TopicNeedsNumberOfPersons(DeltaGreenOriginal, min = 4, max = 6),
        TopicNeedsNumberOfPersons(BladesInTheDark, min = 4, max = 6),
        TopicNeedsNumberOfPersons(SkyrealmsOfJorune, min = 4, max = 6),
        TopicNeedsNumberOfPersons(Wurm, min = 4, max = 6),
        TopicNeedsNumberOfPersons(PsiRun, min = 4, max = 5),
        TopicNeedsNumberOfPersons(Coriolis, min = 4, max = 6),
        TopicNeedsNumberOfPersons(MexicanDeathTrip, min = 4, max = 6),
        TopicNeedsNumberOfPersons(Meute, min = 4, max = 6),
        TopicNeedsNumberOfPersons(LesDerniers, min = 4, max = 6),
        TopicNeedsNumberOfPersons(ChiensDeGuerre, min = 4, max = 6),
        TopicNeedsNumberOfPersons(TalesFromTheLoop, min = 4, max = 6),
        TopicNeedsNumberOfPersons(DonjonCie, min = 4, max = 6),
        TopicNeedsNumberOfPersons(PunchUnit, min = 4, max = 5),
        TopicNeedsNumberOfPersons(DeltaGreenPennsylvania, min = 4, max = 6),
        TopicNeedsNumberOfPersons(Inflorenza, min = 4, max = 5),
        TopicNeedsNumberOfPersons(KpdpDansLeDodecaedre, min = 4, max = 6),
        TopicNeedsNumberOfPersons(SummerCamp, min = 4, max = 7),
        TopicNeedsNumberOfPersons(DieuxEnnemis, min = 4, max = 7),
        TopicNeedsNumberOfPersons(Wastburg, min = 4, max = 6),
        TopicNeedsNumberOfPersons(BlackMirror, min = 4, max = 5),
        TopicNeedsNumberOfPersons(LeRetourDeSoth, min = 4, max = 5),
        TopicNeedsNumberOfPersons(Godbound, min = 4, max = 5),
        TopicNeedsNumberOfPersons(BurningWheel, min = 4, max = 6),
        TopicNeedsNumberOfPersons(EndOfLine, min = 4, max = 6),
        TopicNeedsNumberOfPersons(AuxMarchesDuPouvoir, min = 4, max = 6),
        TopicNeedsNumberOfPersons(DccFunnel, min = 4, max = 6),
        TopicNeedsNumberOfPersons(TortuesNinja, min = 5, max = 5),
        TopicNeedsNumberOfPersons(Heroiques, min = 4, max = 6)
      )

    val All: Set[Constraint] = AllObligations ++ AllInterdictions ++ AllAbsences ++ AllNumbers
  }

  object Preferences {

    import Persons._
    import Topics._

    private val preferenceTree = Set(
      Aude -> Set(
        (DeltaGreenPennsylvania, 1.0),
        (DeltaGreenOriginal, 1.0),
        (DonjonCie, 5.0),
        (Godbound, 1.0),
        (Inflorenza, 1.0),
        (LesSchtroumpfs, 1.0),
        (MexicanDeathTrip, 1.0),
        (ShadowOfTheDemonLord, 1.0),
        (TalesFromTheLoop, 5.0),
        (Wastburg, 5.0),
        (Wurm, 1.0),
      ),
      Bashar -> Set(
        (Agon, 5.0),
        (DcComics, 1.0),
        (DeltaGreenPennsylvania, 1.0),
        (DeltaGreenOriginal, 1.0),
        (DieuxEnnemis, 1.0),
        (Godbound, 1.0),
        (LeRetourDeSoth, 1.0),
        (PunchUnit, 1.0),
        (PsiRun, 1.0),
        (TalesFromTheLoop, 5.0),
      ),
      Boojum -> Set(
        (BlackMirror, 5.0),
        (DcComics, 1.0),
        (DonjonCie, 5.0),
        (Godbound, 1.0),
        (KpdpDansLeDodecaedre, 1.0),
        (MexicanDeathTrip, 1.0),
        (ShadowOfTheDemonLord, 1.0),
        (TalesFromTheLoop, 5.0),
      ),
      Chestel -> Set(
        (DcComics, 5.0),
        (DeltaGreenOriginal, 1.0),
        (Inflorenza, 5.0),
        (LeRetourDeSoth, 1.0),
        (LesDerniers, 1.0),
        (SkyrealmsOfJorune, 5.0),
        (SummerCamp, 1.0),
        (TalesFromTheLoop, 1.0),
      ),
      Cryoban -> Set(
        (BladesInTheDark, 1.0),
        (Coriolis, 1.0),
        (DcComics, 5.0),
        (DonjonCie, 1.0),
        (LesSchtroumpfs, 1.0),
        (Meute, 5.0),
        (Wastburg, 5.0),
        (Wurm, 1.0),
      ),
      Eugenie -> Set(
        (ChiensDeGuerre, 5.0),
        (DeltaGreenOriginal, 1.0),
        (KpdpDansLeDodecaedre, 5.0),
        (PsiRun, 1.0),
        (TalesFromTheLoop, 1.0),
        (Wastburg, 1.0),
      ),
      Gabzeta -> Set(
        (BlackMirror, 1.0),
        (Coriolis, 1.0),
        (DonjonCie, 1.0),
        (Godbound, 1.0),
        (Inflorenza, 1.0),
        (KpdpDansLeDodecaedre, 1.0),
        (LeRetourDeSoth, 1.0),
        (LesSchtroumpfs, 5.0),
        (Meute, 5.0),
        (MexicanDeathTrip, 1.0),
        (ShadowOfTheDemonLord, 1.0),
        (SummerCamp, 1.0),
        (TalesFromTheLoop, 1.0),
        (Wastburg, 5.0),
      ),
      Goat -> Set(
        (BladesInTheDark, 5.0),
        (DieuxEnnemis, 1.0),
        (Godbound, 5.0),
        (ShadowOfTheDemonLord, 5.0),
      ),
      Highlandjul -> Set(
        (BlackMirror, 1.0),
        (BladesInTheDark, 1.0),
        (DcComics, 1.0),
        (DonjonCie, 5.0),
        (Inflorenza, 1.0),
        (LesDerniers, 1.0),
        (Meute, 5.0),
        (SummerCamp, 1.0),
        (Wastburg, 1.0),
      ),
      Isidore -> Set(
        (BladesInTheDark, 1.0),
        (ChiensDeGuerre, 5.0),
        (DeltaGreenPennsylvania, 1.0),
        (DieuxEnnemis, 1.0),
        (DonjonCie, 1.0),
        (LeRetourDeSoth, 1.0),
        (LesSchtroumpfs, 1.0),
        (Meute, 1.0),
        (PsiRun, 1.0),
        (SkyrealmsOfJorune, 5.0),
        (TalesFromTheLoop, 5.0),
      ),
      Jorune -> Set(
        (BlackMirror, 5.0),
        (ChiensDeGuerre, 1.0),
        (DcComics, 5.0),
        (DonjonCie, 1.0),
        (Godbound, 1.0),
        (LesDerniers, 1.0),
        (LesSchtroumpfs, 1.0),
        (ShadowOfTheDemonLord, 1.0),
        (SkyrealmsOfJorune, 5.0),
        (Wurm, 1.0),
      ),
      Julian -> Set(
        (BladesInTheDark, 1.0),
        (Coriolis, 1.0),
        (DcComics, 1.0),
        (DonjonCie, 1.0),
        (LesDerniers, 1.0),
        (PsiRun, 1.0),
      ),
      Kamiseito -> Set(
        (Agon, 5.0),
        (DieuxEnnemis, 1.0),
        (DonjonCie, 1.0),
        (LesDerniers, 5.0),
        (Meute, 1.0),
        (MexicanDeathTrip, 1.0),
        (PunchUnit, 1.0),
        (TalesFromTheLoop, 5.0),
      ),
      Kersa -> Set(
        (ChiensDeGuerre, 1.0),
        (Inflorenza, 1.0),
        (KpdpDansLeDodecaedre, 1.0),
        (LeRetourDeSoth, 1.0),
        (LesDerniers, 5.0),
        (Meute, 1.0),
        (PunchUnit, 5.0),
        (PsiRun, 1.0),
        (Wastburg, 5.0),
      ),
      Killerklown -> Set(
        (BlackMirror, 1.0),
        (BladesInTheDark, 1.0),
        (ChiensDeGuerre, 1.0),
        (DeltaGreenPennsylvania, 1.0),
        (DonjonCie, 1.0),
        (KpdpDansLeDodecaedre, 1.0),
        (LeRetourDeSoth, 1.0),
        (Meute, 5.0),
        (MexicanDeathTrip, 1.0),
        (PunchUnit, 1.0),
        (SkyrealmsOfJorune, 1.0),
        (TalesFromTheLoop, 5.0),
        (Wastburg, 1.0),
      ),
      Mangon -> Set(
        (ChiensDeGuerre, 1.0),
        (DeltaGreenPennsylvania, 1.0),
        (DonjonCie, 5.0),
        (PunchUnit, 1.0),
        (SkyrealmsOfJorune, 5.0),
      ),
      Najael -> Set(
        (BladesInTheDark, 1.0),
        (Coriolis, 1.0),
        (DeltaGreenOriginal, 1.0),
        (DonjonCie, 1.0),
        (KpdpDansLeDodecaedre, 5.0),
        (MexicanDeathTrip, 5.0),
        (TalesFromTheLoop, 5.0),
        (Wastburg, 1.0),
      ),
      Orfeo -> Set(
        (BladesInTheDark, 5.0),
        (ChiensDeGuerre, 1.0),
        (Coriolis, 1.0),
        (Inflorenza, 5.0),
        (KpdpDansLeDodecaedre, 1.0),
        (MexicanDeathTrip, 1.0),
        (ShadowOfTheDemonLord, 1.0),
        (Wurm, 1.0),
      ),
      Ozen -> Set(
        (Agon, 5.0),
        (BlackMirror, 5.0),
        (DieuxEnnemis, 1.0),
        (DonjonCie, 1.0),
        (LesDerniers, 1.0),
        (PsiRun, 5.0),
      ),
      Paiji -> Set(
        (BladesInTheDark, 1.0),
        (Coriolis, 1.0),
        (DeltaGreenPennsylvania, 5.0),
        (DonjonCie, 1.0),
        (Godbound, 1.0),
        (KpdpDansLeDodecaedre, 5.0),
        (LesDerniers, 1.0),
        (PunchUnit, 1.0),
        (ShadowOfTheDemonLord, 1.0),
        (SkyrealmsOfJorune, 5.0),
        (Wastburg, 1.0),
      ),
      Paradoks -> Set(
        (BladesInTheDark, 5.0),
        (Coriolis, 1.0),
        (Godbound, 1.0),
        (Inflorenza, 5.0),
        (LesDerniers, 1.0),
        (MexicanDeathTrip, 5.0),
        (TalesFromTheLoop, 1.0),
        (Wastburg, 1.0),
        (Wurm, 1.0),
      ),
      Rolapin -> Set(
        (Coriolis, 1.0),
        (DeltaGreenPennsylvania, 1.0),
        (Godbound, 1.0),
        (MexicanDeathTrip, 5.0),
        (PunchUnit, 1.0),
        (PsiRun, 1.0),
        (ShadowOfTheDemonLord, 5.0),
        (SkyrealmsOfJorune, 1.0),
        (SummerCamp, 1.0),
        (TalesFromTheLoop, 1.0),
        (Wastburg, 1.0),
      ),
      Sammael99 -> Set(
        (Coriolis, 5.0),
        (DeltaGreenOriginal, 1.0),
        (DieuxEnnemis, 1.0),
        (Godbound, 5.0),
        (LesSchtroumpfs, 1.0),
        (Meute, 5.0),
        (SummerCamp, 1.0),
        (Wastburg, 1.0),
        (Wurm, 1.0),
      ),
      Selpoivre -> Set(
        (BlackMirror, 1.0),
        (BladesInTheDark, 1.0),
        (Coriolis, 1.0),
        (DonjonCie, 1.0),
        (Inflorenza, 1.0),
        (LesDerniers, 1.0),
        (Meute, 1.0),
        (MexicanDeathTrip, 1.0),
        (TalesFromTheLoop, 5.0),
        (Wastburg, 5.0),
        (Wurm, 1.0),
      ),
      Tolkraft -> Set(
        (ChiensDeGuerre, 1.0),
        (DeltaGreenPennsylvania, 5.0),
        (DonjonCie, 1.0),
        (Inflorenza, 1.0),
        (Meute, 1.0),
        (MexicanDeathTrip, 1.0),
        (ShadowOfTheDemonLord, 1.0),
        (Wastburg, 5.0),
      ),
      Zeben -> Set(
        (Agon, 1.0),
        (BladesInTheDark, 5.0),
        (KpdpDansLeDodecaedre, 5.0),
        (Meute, 1.0),
        (PsiRun, 1.0),
        (TalesFromTheLoop, 5.0),
        (Wastburg, 1.0),
      ),
      UdoFemi -> Set(
        (Agon, 1.0),
        (BlackMirror, 5.0),
        (BladesInTheDark, 5.0),
        (DeltaGreenOriginal, 1.0),
        (DonjonCie, 1.0),
        (KpdpDansLeDodecaedre, 1.0),
        (LeRetourDeSoth, 1.0),
        (LesSchtroumpfs, 1.0),
        (ShadowOfTheDemonLord, 1.0),
        (SkyrealmsOfJorune, 5.0),
        (SummerCamp, 1.0)
      )
    )

    val All: Set[Preference] = preferenceTree.flatMap { case (person, prefs) =>
      prefs.map { case (topic, scoreValue) =>
        PersonTopicPreference(person, topic, Score(scoreValue))
      }
    }
  }

  object Problems {
    val Complete: Problem = {
      val p = new ProblemImpl(Slots.All, Topics.All, Persons.All, Constraints.All, Preferences.All)
      log.debug(s"Complete problem is $p")
      p
    }
  }

  object Solutions {

    import Persons._
    import Topics._
    import Slots._

    private implicit val problem: Problem = Problems.Complete

    val Actual: Schedule = Schedule(
      D1Afternoon(
        Agon(Mangon, Kamiseito, Highlandjul, Ozen, Bashar),
        DcComics(Cryoban, Selpoivre, Chestel, Jorune, Boojum),
        LesSchtroumpfs(Paradoks, Isidore, Gabzeta, Aude, Julian),
        ShadowOfTheDemonLord(Zeben, Paiji, Orfeo, Rolapin, Goat, UdoFemi),
        DeltaGreenOriginal(Sammael99, Kersa, Tolkraft, Killerklown, Najael, Eugenie),
      ),
      D1Evening(
        BladesInTheDark(Zeben, Highlandjul, Orfeo, Rolapin, Goat, UdoFemi),
        SkyrealmsOfJorune(Mangon, Sammael99, Paiji, Isidore, Chestel, Jorune),
        Wurm(Kamiseito, Cryoban, Gabzeta, Selpoivre, Aude),
        PsiRun(Kersa, Paradoks, Ozen, Bashar, Eugenie),
        Coriolis(Tolkraft, Killerklown, Najael, Julian, Boojum),
      ),
      D2Afternoon(
        MexicanDeathTrip(Paradoks, Rolapin, Ozen, Najael, Julian),
        Meute(Sammael99, Cryoban, Gabzeta, Highlandjul, Jorune, Killerklown),
        LesDerniers(Kamiseito, Kersa, Tolkraft, Paiji, Chestel),
        ChiensDeGuerre(Mangon, Isidore, Orfeo, Eugenie, Goat),
        TalesFromTheLoop(Zeben, Bashar, Selpoivre, Aude, Boojum, UdoFemi),
      ),
      D2Evening(
        DonjonCie(Mangon, Sammael99, Highlandjul, Aude, Julian, Boojum),
        PunchUnit(Kamiseito, Kersa, Cryoban, Bashar, Killerklown),
        DeltaGreenPennsylvania(Tolkraft, Paiji, Rolapin, Isidore, Selpoivre, Jorune),
        Inflorenza(Paradoks, Gabzeta, Orfeo, Chestel, Eugenie),
        KpdpDansLeDodecaedre(Zeben, Ozen, Najael, Goat, UdoFemi),
      ),
      D3Afternoon(
        SummerCamp(Paradoks, Chestel, Eugenie, Julian),
        DieuxEnnemis(Zeben, Kamiseito, Highlandjul, Bashar),
        Wastburg(Kersa, Tolkraft, Gabzeta, Selpoivre, Aude, Goat),
        BlackMirror(Mangon, Orfeo, Jorune, UdoFemi),
        LeRetourDeSoth(Isidore, Killerklown, Najael, Boojum),
        Godbound(Sammael99, Cryoban, Paiji, Rolapin)
      )
    )
  }

}


