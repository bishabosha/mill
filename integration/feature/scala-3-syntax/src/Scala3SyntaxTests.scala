package mill.integration

import mill.testkit.UtestIntegrationTestSuite

import utest._

object Scala3SyntaxTests extends UtestIntegrationTestSuite {
  override def debugLog: Boolean = true
  val tests: Tests = Tests {
    test("success") - integrationTest { tester =>
      import tester._
      val res = eval(("resolve", "_"))
      if !res.isSuccess then
        throw new Exception("Failed to resolve")
    }
  }
}
