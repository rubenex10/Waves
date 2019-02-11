package com.wavesplatform.lang.compiler.names
import cats.kernel.Monoid
import com.wavesplatform.lang.Common.{NoShrink, produce}
import com.wavesplatform.lang.compiler.compilerContext
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, StdLibVersion}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class NameDuplicationTest extends FreeSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  val ctx: CompilerContext =
    Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), isTokenContext = false).compilerContext)

  "Contract compilation" - {

    "should succeed when" - {

      "constant and user function argument" in {
        compileOf("""
                    |let x = 42
                    |
                    |func some(y: Boolean, x: Boolean) = {
                    |    !x
                    |}
                    |""") shouldBe 'right
      }

      "constant and callable function argument" in {
        compileOf("""
                    |let x = 42
                    |
                    |@Callable(i)
                    |func some(a: Int, x: String) = {
                    |    WriteSet(List(DataEntry("a", x)))
                    |}
                    |""") shouldBe 'right
      }

      "user function and its argument" in {
        compileOf("""
                    |func sameName(sameName: Boolean) = {
                    |   !sameName
                    |}
                    |""") shouldBe 'right
      }

      "user function and argument; callable annotation bindings and arguments; verifier annotation binding" in {
        //TODO в testnet.ide успешно компилится, но в тесте - "Can't find a function"
        compileOf("""
             |func i(i: Int) = {
             |   i
             |}
             |
             |@Callable(x)
             |func foo(i: Int) = {
             |    WriteSet(List(DataEntry("a", toBase58String(x.contractAddress.bytes))))
             |}
             |
             |@Callable(i)
             |func bar(x: Int) = {
             |    WriteSet(List(DataEntry("a", x + 1)))
             |}
             |
             |@Verifier(i)
             |func verify() = {
             |   i(1) > 0
             |}
             |""") shouldBe 'right
      }

    }

    "should fail when" - {

      "these have the same name:" - {

        "two constants" in {
          compileOf("""
                      |let x = 42
                      |let x = true
                      |""") should produce("already defined")
        }

        "constant and user function" in {
          compileOf("""
                      |let x = 42
                      |
                      |func x() = {
                      |   true
                      |}
                      |""") should produce("already defined")
        }

        "constant and callable function" in {
          compileOf("""
                      |let x = 42
                      |
                      |@Callable(i)
                      |func x() = {
                      |   WriteSet(List(DataEntry("a", "a")))
                      |}
                      |""") should produce("already defined")
        }

        "constant and verifier function" in {
          compileOf("""
                      |let x = 42
                      |
                      |@Verifier(i)
                      |func x() = {
                      |   WriteSet(List(DataEntry("a", "a")))
                      |}
                      |""") should produce("already defined")
        }

        "constant and callable annotation binding" in {
          compileOf("""
                      |let x = 42
                      |
                      |@Callable(x)
                      |func some(i: Int) = {
                      |    WriteSet(List(DataEntry("a", "a")))
                      |}
                      |""") should produce("already defined")
        }

        "constant and verifier annotation binding" in {
          compileOf("""
                      |let x = 42
                      |
                      |@Verifier(x)
                      |func some() = {
                      |    true
                      |}
                      |""") should produce("already defined")
        }

        "two user functions" in {
          compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |func sameName() = {
                    |   1
                    |}
                    |""") should produce("already defined")
        }

        "two user function arguments" in {
          //TODO не хватает пробела в Function'some' declared with duplicating argument names
          compileOf("""
                    |func some(sameName: String, sameName: Int) = {
                    |   sameName
                    |}
                    |""") should produce("already defined")
        }

        "user and callable functions" in {
          compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |@Callable(i)
                    |func sameName() = {
                    |   WriteSet(List(DataEntry("a", "a")))
                    |}
                    |""") should produce("already defined")
        }

        "user and verifier functions" in {
          compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |@Verifier(i)
                    |func sameName() = {
                    |   true
                    |}
                    |""") should produce("already defined")
        }

        "user function and callable annotation binding" in {
          compileOf("""
                      |func i() = {
                      |   true
                      |}
                      |
                      |@Callable(i)
                      |func some() = {
                      |    WriteSet(List(DataEntry("a", i.contractAddress)))
                      |}
                      |""") should produce("already defined")
        }

        "user function and verifier annotation binding" in {
          compileOf("""
                      |func i() = {
                      |   true
                      |}
                      |
                      |@Verifier(i)
                      |func some() = {
                      |    if (i.contractAddress == "abc") then
                      |       true
                      |    else
                      |       false
                      |}
                      |""") should produce("already defined")
        }

        "two callable functions" in {
          compileOf("""
             |@Callable(i)
             |func sameName() = {
             |   WriteSet(List(DataEntry("a", "a")))
             |}
             |
             |@Callable(i)
             |func sameName() = {
             |   WriteSet(List(DataEntry("b", "b")))
             |}
             |""") should produce("already defined")
        }

        "two callable function arguments" in {
          //TODO не хватает пробела в Function'some' declared with duplicating argument names
          compileOf("""
                      |@Callable(i)
                      |func some(sameName: String, sameName: Int) = {
                      |   WriteSet(List(DataEntry("b", sameName)))
                      |}
                      |""") should produce("already defined")
        }

        "callable and verifier functions" in {
          compileOf("""
             |@Callable(i)
             |func sameName() = {
             |   WriteSet(List(DataEntry("a", "a")))
             |}
             |
             |@Verifier(i)
             |func sameName() = {
             |   true
             |}
             |""") should produce("already defined")
        }

        "callable function and its callable annotation binding" in {
          compileOf("""
             |@Callable(sameName)
             |func sameName() = {
             |   WriteSet(List(DataEntry("a", sameName.contractAddress)))
             |}
             |""") shouldBe 'right// produce("already defined")
        }

        "callable annotation binding and its function argument" in {
          compileOf("""
             |@Callable(i)
             |func some(s: String, i: Int) = {
             |   if (i.contractAddress == "abc") then
             |      WriteSet(List(DataEntry("a", "a")))
             |   else
             |      WriteSet(List(DataEntry("a", "b")))
             |}
             |""") should produce("override annotation bindings") //TODO бэд инглиш?
        }

      }

    }

  }

  def compileOf(script: String): Either[String, Contract] = {
    val expr = Parser.parseContract(script.stripMargin).get.value
    compiler.ContractCompiler(ctx, expr)
  }

}
