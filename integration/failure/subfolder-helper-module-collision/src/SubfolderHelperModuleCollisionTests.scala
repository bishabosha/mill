package mill.integration

import mill.testkit.IntegrationTestSuite

import utest._

object SubfolderHelperModuleCollisionTests extends IntegrationTestSuite {
  val tests: Tests = Tests {
    initWorkspace()

    test("success") {
      val res = eval(("resolve", "_"))
      assert(res.isSuccess == false)
      // Not a great error message but it will have to do for now
      assert(res.err.contains("Trying to define package with same name as class sub"))
    }
  }
}
