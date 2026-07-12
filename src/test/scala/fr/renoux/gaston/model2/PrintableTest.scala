package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.model2.Printable.given

class PrintableTest extends TestBase {

  import PrintableTest.*

  "basic types" - {
    "int" in {
      42.toPrettyString should be("42")
    }
    "long" in {
      42L.toPrettyString should be("42")
    }
    "double" in {
      3.14.toPrettyString should be("3.14")
    }
  }

  "various" - {
    "string" in {
      "hello".toPrettyString should be("hello")
    }
    "custom" in {
      Dog("Scooby-Doo").toPrettyString should be ("good dog Scooby-Doo")
    }
  }

  "collections" - {
    "array" in {
      Array(Dog("Scooby-Doo"), Dog("Scrappy-Doo")).toPrettyString should be ("[ good dog Scooby-Doo, good dog Scrappy-Doo ]")
    }
    "list" in {
      List(Dog("Scooby-Doo"), Dog("Scrappy-Doo")).toPrettyString should be ("[ good dog Scooby-Doo, good dog Scrappy-Doo ]")
    }
    "vector" in {
      Vector(Dog("Scooby-Doo"), Dog("Scrappy-Doo")).toPrettyString should be ("[ good dog Scooby-Doo, good dog Scrappy-Doo ]")
    }
    "map" in {
      Map(Cat("Tom") -> Dog("Spike"), Cat("Oliver") -> Dog("Dodger")).toPrettyString should be ("{ cat Tom: good dog Spike, cat Oliver: good dog Dodger }")
    }
  }
}

object PrintableTest {
  class Dog(val name: String)

  object Dog {
    given Printable[Dog] with {
      extension (dog: Dog) {
        override def toPrettyString: String = s"good dog ${dog.name}"
      }
    }
  }

  class Cat(val name: String)

  object Cat {
    given Printable[Cat] with {
      extension (cat: Cat) {
        override def toPrettyString: String = s"cat ${cat.name}"
      }
    }
  }
}
