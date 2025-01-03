package fr.renoux.gaston.util

import fr.renoux.gaston.TestBase

class FastLoopsTest extends TestBase {
  "fastFind" - {
    "nominal" in {
      fastFind(1, 42)(_ % 5 == 0) should be(5)
    }

    "no result" in {
      fastFind(1, 42)(_ % 43 == 0) should be(-1)
    }

    "no result, custom default" in {
      fastFind(1, 42, default = 666)(_ % 43 == 0) should be(666)
    }
  }
}
