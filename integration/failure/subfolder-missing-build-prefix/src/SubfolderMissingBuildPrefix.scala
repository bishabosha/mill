package mill.integration

import mill.testkit.IntegrationTestSuite

import utest._

object SubfolderMissingBuildPrefix extends IntegrationTestSuite {
  val tests: Tests = Tests {
    initWorkspace()

    test("success") {
      val res = eval(("resolve", "_"))
      assert(!res.isSuccess)
      assert(res.err.contains("value y is not a member of build_.sub"))
    }
  }
}
