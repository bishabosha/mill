package mill.main

import mill.api.{PathRef, Result, Val}
import mill.{Agg, T}
import mill.define.{Cross, Discover, Module, Task}
import mill.testkit.UnitTester
import mill.testkit.TestBaseModule
import utest.{TestSuite, Tests, assert, test}

import java.io.{ByteArrayOutputStream, PrintStream}

object MainModuleTests extends TestSuite {

  object mainModule extends TestBaseModule with MainModule {
    def hello = T {
      System.out.println("Hello System Stdout")
      System.err.println("Hello System Stderr")
      Console.out.println("Hello Console Stdout")
      Console.err.println("Hello Console Stderr")
      Seq("hello", "world")
    }
    def hello2 = T {
      System.out.println("Hello2 System Stdout")
      System.err.println("Hello2 System Stderr")
      Console.out.println("Hello2 Console Stdout")
      Console.err.println("Hello2 Console Stderr")
      Map("1" -> "hello", "2" -> "world")
    }
    def helloCommand(x: Int, y: Task[String]) = T.command { (x, y(), hello()) }
    import mill.main.TokenReaders.given
    override lazy val millDiscover: Discover[this.type] = Discover[this.type]
  }

  object cleanModule extends TestBaseModule with MainModule {

    trait Cleanable extends Module {
      def target = T {
        os.write(T.dest / "dummy.txt", "dummy text")
        Seq(PathRef(T.dest))
      }
    }

    object foo extends Cleanable {
      object sub extends Cleanable
    }
    object bar extends Cleanable {
      override def target = T {
        os.write(T.dest / "dummy.txt", "dummy text")
        super.target() ++ Seq(PathRef(T.dest))
      }
    }
    object bazz extends Cross[Bazz]("1", "2", "3")
    trait Bazz extends Cleanable with Cross.Module[String]

    def all = T {
      foo.target()
      bar.target()
      bazz("1").target()
      bazz("2").target()
      bazz("3").target()
    }
  }

  override def tests: Tests = Tests {

    test("inspect") {
      val eval = UnitTester(mainModule, null)
      test("single") {
        val res = eval.evaluator.evaluate(Agg(mainModule.inspect(eval.evaluator, "hello")))
        val Result.Success(Val(value: String)) = res.rawValues.head
        assert(
          res.failing.keyCount == 0,
          value.startsWith("hello("),
          value.contains("MainModuleTests.scala:")
        )
      }
      test("multi") {
        val res =
          eval.evaluator.evaluate(Agg(mainModule.inspect(eval.evaluator, "hello", "hello2")))
        val Result.Success(Val(value: String)) = res.rawValues.head
        assert(
          res.failing.keyCount == 0,
          value.startsWith("hello("),
          value.contains("MainModuleTests.scala:"),
          value.contains("\n\nhello2(")
        )
      }
      test("command") {
        val Right(result) = eval.apply("inspect", "helloCommand")

        val Seq(res: String) = result.value
        assert(
          res.startsWith("helloCommand("),
          res.contains("MainModuleTests.scala:"),
          res.contains("hello")
        )
      }
    }

    test("show") {
      val outStream = new ByteArrayOutputStream()
      val errStream = new ByteArrayOutputStream()
      val evaluator = UnitTester(
        mainModule,
        null,
        outStream = new PrintStream(outStream, true),
        errStream = new PrintStream(errStream, true)
      )
      test("single") {
        val results =
          evaluator.evaluator.evaluate(Agg(mainModule.show(evaluator.evaluator, "hello")))

        assert(results.failing.keyCount == 0)

        val Result.Success(Val(value)) = results.rawValues.head

        val shown = ujson.read(outStream.toByteArray)
        val expected = ujson.Arr.from(Seq("hello", "world"))
        assert(value == expected)
        assert(shown == expected)

        // Make sure both stdout and stderr are redirected by `show`
        // to stderr so that only the JSON file value goes to stdout
        val strippedErr =
          fansi.Str(errStream.toString, errorMode = fansi.ErrorMode.Sanitize).plainText

        assert(strippedErr.contains("Hello System Stdout"))
        assert(strippedErr.contains("Hello System Stderr"))
        assert(strippedErr.contains("Hello Console Stdout"))
        assert(strippedErr.contains("Hello Console Stderr"))
      }
      test("multi") {
        val results =
          evaluator.evaluator.evaluate(Agg(mainModule.show(
            evaluator.evaluator,
            "hello",
            "+",
            "hello2"
          )))

        assert(results.failing.keyCount == 0)

        val Result.Success(Val(value)) = results.rawValues.head

        val shown = ujson.read(outStream.toByteArray)

        val expected = ujson.Obj(
          "hello" -> ujson.Arr("hello", "world"),
          "hello2" -> ujson.Obj("1" -> "hello", "2" -> "world")
        )
        assert(value == expected)
        assert(shown == expected)

        // Make sure both stdout and stderr are redirected by `show`
        // to stderr so that only the JSON file value goes to stdout
        val strippedErr =
          fansi.Str(errStream.toString, errorMode = fansi.ErrorMode.Sanitize).plainText

        assert(strippedErr.contains("Hello2 System Stdout"))
        assert(strippedErr.contains("Hello2 System Stderr"))
        assert(strippedErr.contains("Hello2 Console Stdout"))
        assert(strippedErr.contains("Hello2 Console Stderr"))
      }

      test("command") {
        val Left(Result.Failure(failureMsg, _)) = evaluator.apply("show", "helloCommand")
        assert(
          failureMsg.contains("Expected Signature: helloCommand"),
          failureMsg.contains("-x <int>"),
          failureMsg.contains("-y <str>")
        )
        val Right(result) =
          evaluator.apply("show", "helloCommand", "-x", "1337", "-y", "lol")

        val Seq(res) = result.value
        assert(res == ujson.Arr(1337, "lol", ujson.Arr("hello", "world")))
      }
    }

    test("showNamed") {
      val evaluator = UnitTester(mainModule, null)
      test("single") {
        val results =
          evaluator.evaluator.evaluate(Agg(mainModule.showNamed(evaluator.evaluator, "hello")))

        assert(results.failing.keyCount == 0)

        val Result.Success(Val(value)) = results.rawValues.head

        assert(value == ujson.Obj.from(Map(
          "hello" -> ujson.Arr.from(Seq("hello", "world"))
        )))
      }
      test("multi") {
        val results =
          evaluator.evaluator.evaluate(Agg(mainModule.showNamed(
            evaluator.evaluator,
            "hello",
            "+",
            "hello2"
          )))

        assert(results.failing.keyCount == 0)

        val Result.Success(Val(value)) = results.rawValues.head

        assert(value == ujson.Obj.from(Map(
          "hello" -> ujson.Arr.from(Seq("hello", "world")),
          "hello2" -> ujson.Obj.from(Map("1" -> "hello", "2" -> "world"))
        )))
      }
    }

    test("clean") {
      val ev = UnitTester(cleanModule, null)
      val out = ev.evaluator.outPath

      def checkExists(exists: Boolean)(paths: os.SubPath*): Unit = {
        paths.foreach { path =>
          assert(os.exists(out / path) == exists)
        }
      }

      test("all") {
        val r1 = ev.evaluator.evaluate(Agg(cleanModule.all))
        assert(r1.failing.keyCount == 0)
        checkExists(true)(os.sub / "foo")

        val r2 = ev.evaluator.evaluate(Agg(cleanModule.clean(ev.evaluator)))
        assert(r2.failing.keyCount == 0)
        checkExists(false)(os.sub / "foo")
      }

      test("single-target") {
        val r1 = ev.evaluator.evaluate(Agg(cleanModule.all))
        assert(r1.failing.keyCount == 0)
        checkExists(true)(
          os.sub / "foo" / "target.json",
          os.sub / "foo" / "target.dest" / "dummy.txt",
          os.sub / "bar" / "target.json",
          os.sub / "bar" / "target.dest" / "dummy.txt"
        )

        val r2 = ev.evaluator.evaluate(Agg(cleanModule.clean(ev.evaluator, "foo.target")))
        assert(r2.failing.keyCount == 0)
        checkExists(false)(
          os.sub / "foo" / "target.log",
          os.sub / "foo" / "target.json",
          os.sub / "foo" / "target.dest" / "dummy.txt"
        )
        checkExists(true)(
          os.sub / "bar" / "target.json",
          os.sub / "bar" / "target.dest" / "dummy.txt"
        )
      }

      test("single-module") {
        val r1 = ev.evaluator.evaluate(Agg(cleanModule.all))
        assert(r1.failing.keyCount == 0)
        checkExists(true)(
          os.sub / "foo" / "target.json",
          os.sub / "foo" / "target.dest" / "dummy.txt",
          os.sub / "bar" / "target.json",
          os.sub / "bar" / "target.dest" / "dummy.txt"
        )

        val r2 = ev.evaluator.evaluate(Agg(cleanModule.clean(ev.evaluator, "bar")))
        assert(r2.failing.keyCount == 0)
        checkExists(true)(
          os.sub / "foo" / "target.json",
          os.sub / "foo" / "target.dest" / "dummy.txt"
        )
        checkExists(false)(
          os.sub / "bar" / "target.json",
          os.sub / "bar" / "target.dest" / "dummy.txt"
        )
      }
    }
  }
}
